# our packages overlay
pkgs: _: with pkgs; {
  cardanoRestHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
