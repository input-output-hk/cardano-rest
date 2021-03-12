{ pkgs, lib, iohkNix, customConfig }:
let
  blacklistedEnvs = [ "selfnode" "shelley_selfnode" "latency-tests" "mainnet-ci" "shelley_testnet" ];
  environments = lib.filterAttrs (k: v: (!builtins.elem k blacklistedEnvs)) iohkNix.cardanoLib.environments;
  mkStartScripts = envConfig: let
    extraModule = {
      services.cardano-explorer-api = {
        enable = true;
        postgres.user = "*";
      };
    };
    systemdCompat.options = {
      systemd.services = lib.mkOption {};
      services.postgresql = lib.mkOption {};
      users = lib.mkOption {};
    };
    eval = lib.evalModules {
      prefix = [];
      modules = import nixos/module-list.nix ++ [ systemdCompat extraModule customConfig ];
      args = { inherit pkgs; };
    };
  in {
    explorer-api = eval.config.services.cardano-explorer-api.script;
  };
in iohkNix.cardanoLib.forEnvironmentsCustom mkStartScripts environments // { inherit environments; }
