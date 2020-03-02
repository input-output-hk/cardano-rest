{ config, lib, pkgs, ... }:

# notes:
# this service exposes an http port, and connects to a cardano-node over a unix socket
let
  cfg = config.services.cardano-tx-submit;
  envConfig = cfg.environment;
  localPkgs = import ../. {};
in {
  options = {
    services.cardano-tx-submit = {
      enable = lib.mkEnableOption "enable the cardano-tx-submit api";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = localPkgs.cardanoRestHaskellPackages.cardano-tx-submit.components.exes.cardano-tx-submit-webapi;
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 8101;
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
      network = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "network name";
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = localPkgs.iohkNix.cardanoLib.environments.${cfg.network};
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.cardano-tx-submit.script = pkgs.writeShellScript "cardano-tx-submit" ''
      ${if (cfg.socketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
      then
        echo "You must set \$CARDANO_NODE_SOCKET_PATH"
        exit 1
      fi'' else "export \"CARDANO_NODE_SOCKET_PATH=${cfg.socketPath}\""}
      exec ${cfg.package}/bin/cardano-tx-submit-webapi --socket-path "$CARDANO_NODE_SOCKET_PATH" \
            --genesis-file ${envConfig.genesisFile} \
            --port ${toString cfg.port} \
            --config ${builtins.toFile "tx-submit.json" (builtins.toJSON cfg.environment.txSubmitConfig)}
    '';
    systemd.services.cardano-tx-submit = {
      serviceConfig = {
        ExecStart = config.services.cardano-tx-submit.script;
        DynamicUser = true;
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "cardano-node.service" ];
    };
  };
}
