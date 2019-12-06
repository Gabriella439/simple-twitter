# `simple-twitter`

This is a project which has corresponding [talk slides](https://github.com/Gabriel439/slides/blob/master/simple-twitter/slides.md)

This is a bare bones "Twitter clone" implemented in
[a **single file**](./simple-twitter.nix) that you can deploy to EC2

This uses:

* HTML + CSS for the front-end (No JavaScript)
* [Bootstrap](https://getbootstrap.com) for some minimal styling
* A [Haskell](https://www.haskell.org) service for server-side rendering
* A [Postgres database](https://www.postgresql.org) for application state
* [NixOps](https://nixos.org/nixops/manual/) for deployment
* [AWS EC2](https://aws.amazon.com/ec2/) for hosting

The result looks like this:

![Screenshot](https://user-images.githubusercontent.com/1313787/69443895-25987480-0d04-11ea-8260-8cfd2757f219.png)

## Instructions

1.  Create an AWS account

    ... by following [these instructions](https://aws.amazon.com/premiumsupport/knowledge-center/create-and-activate-aws-account/)

1.  Install Nix:

    ```bash
    $ curl https://nixos.org/nix/install | sh
    ```

1.  Install the AWS command-line interface:

    ```bash
    $ nix-env --install awscli
    ```

1.  Configure your AWS credentials

    ... by following [these instructions](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html)

    If you did this correctly you should have an `~/.aws/credentials`
    file that looks similar to this:

    ```ini
    [default]
    aws_access_key_id = …
    aws_secret_access_key = …
    ```

1.  Install NixOps:

    ```bash
    $ nix-env --install nixops
    ```

1.  Build and redeploy the web application

    ```bash
    $ nixops create --deployment simple-twitter simple-twitter.nix
    $ nixops deploy --deployment simple-twitter --allow-reboot
    ```

If you make changes you can redeploy the application by re-running the last
step:

```bash
$ nixops deploy --deployment simple-twitter --allow-reboot
```

To destroy the machine and clean up everything, run:

```bash
$ nixops destroy --deployment simple-twitter
$ nixops delete --deployment simple-twitter
```

Have fun! 🙂

## Split files

You can also view the files split out by language:

* [`./split.nix`](./split.nix) - Only the Nix code
* [`./Main.hs`](./Main.hs) - Only the Haskell code
* [`./initialScript.sql`](./initialScript.sql) - Only the SQL code
