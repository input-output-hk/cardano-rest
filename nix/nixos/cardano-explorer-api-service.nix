{ config, lib, pkgs, ... }:

let
  cfg = config.services.cardano-explorer-api;
in {
  options = {
    services.cardano-explorer-api = {
      enable = lib.mkEnableOption "enable the cardano-explorer web api";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 8100;
      };
      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };
      pgpass = lib.mkOption {
        internal = true;
        type = lib.types.path;
      };
      pgpassFile = lib.mkOption {
        type = lib.types.path;
        default = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = (import ../. {}).cardanoRestHaskellPackages.cardano-explorer-api.components.exes.cardano-explorer-api;
      };
      postgres = {
        socketdir = lib.mkOption {
          type = lib.types.str;
          default = "/run/postgresql";
          description = "the path to the postgresql socket";
        };
        port = lib.mkOption {
          type = lib.types.int;
          default = 5432;
          description = "the postgresql port";
        };
        database = lib.mkOption {
          type = lib.types.str;
          default = "cexplorer";
          description = "the postgresql database to use";
        };
        user = lib.mkOption {
          type = lib.types.str;
          default = "cexplorer";
          description = "the postgresql user to use";
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.cardano-explorer-api = {
      script = pkgs.writeShellScript "cardano-explorer-api" ''
        export PGPASSFILE=${cfg.pgpassFile}
        exec ${cfg.package}/bin/cardano-explorer-api \
          --port ${toString cfg.port} \
          --listen-address ${cfg.listenAddress}
      '';
    };
    systemd.services.cardano-explorer-api = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      path = [ pkgs.netcat ];
      preStart = ''
        for x in {1..10}; do
          nc -z localhost ${toString cfg.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
      '';
      serviceConfig = {
        ExecStart = cfg.script;
        User = "cexplorer";
      };
    };
  };
}
