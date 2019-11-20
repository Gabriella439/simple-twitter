let
  region = "us-west-1";

  accessKeyId = "default";

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

        initialScript = ./initialScript.sql;
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

            code = ./Main.hs;

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
