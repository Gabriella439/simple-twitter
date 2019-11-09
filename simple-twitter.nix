let
  region = "us-west-1";

  accessKeyId = "personal";

in
  { machine = { config, pkgs, resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit accessKeyId region;

          instanceType = "t2.nano";

          keyPair = resources.ec2KeyPairs.my-key-pair;
        };
      };

      networking.firewall.allowedTCPPorts = [ 80 ];

      services.postgresql = {
        enable = true;

        authentication = ''
          local all all ident map=mapping
        '';

        identMap = ''
          mapping root     postgres
          mapping postgres postgres
        '';

        package = pkgs.postgresql_11;

        initialScript = pkgs.writeText "initialScript.sql" ''
          CREATE TABLE "user" (
            name text NOT NULL,
            PRIMARY KEY (name)
          );

          CREATE TABLE tweet (
            id integer GENERATED ALWAYS AS IDENTITY,
            contents text NOT NULL,
            time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (id)
          );

          CREATE TABLE user_tweet (
            "user" text NOT NULL,
            tweet integer NOT NULL,
            PRIMARY KEY ("user", tweet),
            FOREIGN KEY ("user") REFERENCES "user" (name) ON DELETE CASCADE,
            FOREIGN KEY (tweet) REFERENCES tweet (id) ON DELETE CASCADE
          );

          CREATE TABLE follows (
            follower text NOT NULL,
            followed text NOT NULL,
            PRIMARY KEY (follower, followed),
            FOREIGN KEY (follower) REFERENCES "user" (name) ON DELETE CASCADE,
            FOREIGN KEY (followed) REFERENCES "user" (name) ON DELETE CASCADE
          );
        '';
      };

      systemd.services.simple-twitter = {
        wantedBy = [ "multi-user.target" ];

        after = [ "postgresql.service" ];

        script =
          let
            ghc =
              pkgs.haskellPackages.ghcWithPackages (pkgs: [
                  pkgs.blaze-html
                  pkgs.blaze-markup
                  pkgs.exceptions
                  pkgs.http-api-data
                  pkgs.optparse-generic
                  pkgs.postgresql-simple
                  pkgs.servant
                  pkgs.servant-blaze
                  pkgs.servant-server
                  pkgs.text
                  pkgs.transformers
                  pkgs.warp
                ]
              );

            code = pkgs.writeText "Main.hs" ''
{-# LINE 90 "Main.hs" #-}
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
                    SELECT "user".name, tweet.contents
                    FROM           "user"
                        INNER JOIN user_tweet ON "user".name = user_tweet."user"
                        INNER JOIN tweet      ON user_tweet.tweet = tweet.id
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
                        FROM           "user"
                            INNER JOIN follows ON "user".name = follows.follower
                        WHERE "user".name = ?
                    |]

                    history <- query user [sql|
                        SELECT "user".name, tweet.contents
                        FROM           "user"
                            INNER JOIN user_tweet ON "user".name = user_tweet."user"
                            INNER JOIN tweet      ON user_tweet.tweet = tweet.id
                        WHERE "user".name = ?
                        ORDER BY tweet.time DESC
                    |]

                    timeline <- query user [sql|
                        SELECT follows.followed, tweet.contents
                        FROM           "user"
                            INNER JOIN follows    ON "user".name = follows.follower
                            INNER JOIN user_tweet ON follows.followed = user_tweet."user"
                            INNER JOIN tweet      ON user_tweet.tweet = tweet.id
                        WHERE "user".name = ?
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

                execute (Only name :. id) [sql|
                    INSERT INTO user_tweet ("user", tweet) VALUES (?, ?)
                |]

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
            '';

            simple-twitter = pkgs.runCommand "simple-twitter" {} ''
              ${pkgs.coreutils}/bin/mkdir --parents $out/bin

              ${ghc}/bin/ghc -O -Wall -Werror ${code} -o $out/bin/simple-twitter
            '';

          in
            ''
            ${simple-twitter}/bin/simple-twitter --connectPort ${toString config.services.postgresql.port}
            '';
      };
    };

    resources = {
      ec2KeyPairs.my-key-pair = { inherit region accessKeyId; };

      ec2SecurityGroups."http" = {
        inherit accessKeyId region;

        rules = [
          { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
        ];
      };
    };
  }
