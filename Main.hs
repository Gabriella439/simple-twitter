{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Data.Word (Word16)
import GHC.Generics (Generic)
import Options.Generic (ParseRecord)
import Prelude hiding (id)
import Database.PostgreSQL.Simple (FromRow, Only(..), Query, ToRow, (:.)(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Servant.HTML.Blaze (HTML)
import Servant.Server (Handler)
import Text.Blaze (Markup, (!))
import Text.Blaze.Html5 (AttributeValue)
import Web.HttpApiData (FromHttpApiData)
import Web.FormUrlEncoded (FromForm)

import Servant.API
    ( FormUrlEncoded
    , Get
    , Post
    , QueryParam'
    , ReqBody
    , Required
    , Strict
    , (:>)
    , (:<|>)(..)
    )

import qualified Control.Exception           as Exception
import qualified Control.Monad               as Monad
import qualified Control.Monad.Catch         as Catch
import qualified Database.PostgreSQL.Simple  as PostgreSQL
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Options.Generic             as Options
import qualified Servant.Server              as Server
import qualified Text.Blaze.Html5            as Html
import qualified Text.Blaze.Html5.Attributes as Attr

newtype Options = Options { connectPort :: Word16 }
    deriving stock (Generic)
    deriving anyclass (ParseRecord)

newtype User = User { name :: Text }
    deriving stock (Generic)
    deriving anyclass (FromForm, FromRow, ToRow)
    deriving newtype (FromHttpApiData)

data Follow = Follow { follower :: Text, followed :: Text }
    deriving stock (Generic)
    deriving anyclass (FromForm, ToRow)

data Tweet = Tweet { name :: Text, contents :: Text }
    deriving stock (Generic)
    deriving anyclass (FromForm, FromRow)

type API =
        Get '[HTML] Markup
   :<|> "user" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Markup
   :<|> "user" :> QueryParam' '[Required, Strict] "name" User :> Get '[HTML] Markup
   :<|> "user" :> "delete" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Markup
   :<|> "users" :> Get '[HTML] Markup
   :<|> "tweet" :> ReqBody '[FormUrlEncoded] Tweet :> Post '[HTML] Markup
   :<|> "follow" :> ReqBody '[FormUrlEncoded] Follow :> Post '[HTML] Markup

main :: IO ()
main = do
    Options {..} <- Options.getRecord "Simple Twitter"

    let connectInfo =
            PostgreSQL.defaultConnectInfo
              { PostgreSQL.connectPort = connectPort
              , PostgreSQL.connectHost = ""
              }

    let open = PostgreSQL.connect connectInfo
    let close = PostgreSQL.close

    Exception.bracket open close \connection -> do
        let execute :: (MonadIO io, ToRow input) => input -> Query -> io ()
            execute input q = liftIO do
                _ <- PostgreSQL.execute connection q input

                return ()

        let query
                :: (MonadIO io, ToRow input, FromRow output)
                => input -> Query -> io [output]
            query input q = liftIO (PostgreSQL.query connection q input)

        let query_
                :: (MonadIO io, FromRow output)
                => Query -> io [output]
            query_ q = liftIO (PostgreSQL.query_ connection q)

        let submit label =
                    Html.button
                !   Attr.type_ "submit"
                !   Attr.class_ "btn btn-primary btn-sm"
                $   label

        let field :: AttributeValue -> Markup
            field name = do
                Html.div ! Attr.class_ "form-group" $ do
                    Html.input
                        !   Attr.type_ "text"
                        !   Attr.class_ "form-control form-control-sm"
                        !   Attr.name name
                        !   Attr.placeholder name

        let form action method html =
                Html.div ! Attr.class_ "col-md-4" $ do
                    Html.form
                        !   Attr.action action
                        !   Attr.method method
                        !   Attr.class_ "border m-3 p-2 bg-light"
                        $   html

        let forms :: Markup
            forms = do
                Html.div ! Attr.class_ "row" $ do
                    form "/" "get" do
                        submit "Global timeline"

                Html.div ! Attr.class_ "row" $ do
                    form "/user" "post" do
                        field "name"
                        submit "Create user"

                    form "/user/delete" "post" do
                        field "name"
                        submit "Delete user"

                    form "/users" "get" do
                        submit "Get users"

                Html.div ! Attr.class_ "row" $ do
                    form "/tweet" "post" do
                        field "name"
                        field "contents"
                        submit "Create tweet"

                    form "/follow" "post" do
                        field "follower"
                        field "followed"
                        submit "Follow"

                    form "/user" "get" do
                        field "name"
                        submit "Get user"

        let ul html = Html.ul ! Attr.class_ "list-group" $ html

        let li html = Html.li ! Attr.class_ "list-group-item" $ html

        let wrap :: Markup -> Markup
            wrap body =
                Html.html do
                    Html.head do
                        Html.title "Simple Twitter"
                        Html.link
                            ! Attr.rel "stylesheet"
                            ! Attr.href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"

                    Html.body do
                        Html.h1
                            ! Attr.class_ "display-4 text-center"
                            $ "Simple Twitter"

                        Html.div ! Attr.class_ "container" $ do
                          Html.div ! Attr.class_ "row" $ do
                              Html.div ! Attr.class_ "col-md-6" $ forms
                              Html.div ! Attr.class_ "col-md-6" $ body

        let failWith :: Text -> Handler Markup -> Handler Markup
            failWith message handler = do
                let fallback :: SomeException -> Handler Markup
                    fallback _ = return (wrap (Html.toHtml message))

                Catch.handle fallback handler

        let index :: Handler Markup
            index = do
                tweets <- query_ [sql|
                    SELECT tweet.author, tweet.contents
                    FROM tweet
                    ORDER BY tweet.time DESC
                |]

                let renderTweet (Tweet {..}) =
                        li (Html.toHtml (name <> ": " <> contents))

                return do
                    wrap (ul (traverse_ renderTweet tweets))

        let getUsers :: Handler Markup
            getUsers = do
                users <- query_ [sql|SELECT name FROM "user"|]

                let renderUser (User {..}) = li (Html.toHtml name)

                return (wrap (ul (traverse_ renderUser users)))

        let createUser :: User -> Handler Markup
            createUser user@User{..} = do
                let message =
                        "A user named '" <> name <> "' already exists"

                failWith message do
                    execute user [sql|INSERT INTO "user" (name) VALUES (?)|]

                    getUsers

        let getUser :: User -> Handler Markup
            getUser user@User{..} = do
                let message =
                        "No user named '" <> name <> "' exists"

                failWith message do
                    followeds <- query user [sql|
                        SELECT follows.followed
                        FROM follows
                        WHERE follows.follower = ?
                    |]

                    history <- query user [sql|
                        SELECT tweet.author, tweet.contents
                        FROM tweet
                        WHERE tweet.author = ?
                        ORDER BY tweet.time DESC
                    |]

                    timeline <- query user [sql|
                        SELECT follows.followed, tweet.contents
                        FROM           follows
                            INNER JOIN tweet ON follows.followed = tweet.author
                        WHERE follows.follower = ?
                        ORDER BY tweet.time DESC
                    |]

                    let renderHistory (Tweet { contents }) =
                            li (Html.toHtml contents)

                    let renderTimeline (Tweet { name = followed, ..}) =
                            li (Html.toHtml (followed <> ": " <> contents))

                    let renderUser (User { name = followed }) =
                            li (Html.toHtml followed)

                    return do
                        wrap do
                            Html.h2 (Html.toHtml name)
                            Html.hr
                            Monad.when (not (null history)) do
                                Html.h3 "History"
                                ul (traverse_ renderHistory history)

                            Monad.when (not (null timeline)) do
                                Html.h3 "Timeline"
                                ul (traverse_ renderTimeline timeline)

                            Monad.when (not (null followeds)) do
                                Html.h3 "Follows"
                                ul (traverse_ renderUser followeds)

        let deleteUser :: User -> Handler Markup
            deleteUser user@User{..}= do
                let message =
                        "No user named '" <> name <> "' exists"

                failWith message do
                    execute user [sql|DELETE FROM "user" WHERE name = ?|]

                    getUsers

        let createTweet :: Tweet -> Handler Markup
            createTweet (Tweet {..}) = do
                rows <- query (Only contents) [sql|
                    INSERT INTO tweet (contents) VALUES (?) RETURNING (id)
                |]

                id <- case rows of
                    [ (id :: Only Integer) ] -> return id
                    _                        -> Catch.throwM Server.err500

                getUser (User {..})

        let follow :: Follow -> Handler Markup
            follow f@Follow{..} = do
                let message =
                        "'" <> follower <> "' already follows '" <> followed <> "'"
                failWith message do
                    execute f [sql|
                        INSERT INTO follows (follower, followed) VALUES (?, ?)
                    |]

                    getUser (User { name = follower })

        let server = index
                :<|> createUser
                :<|> getUser
                :<|> deleteUser
                :<|> getUsers
                :<|> createTweet
                :<|> follow

        let application = Server.serve @API Proxy server

        Warp.run 80 application
