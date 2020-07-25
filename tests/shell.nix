{ pkgs ? import <nixpkgs> {} }:
let
  myPython = pkgs.python38.withPackages (ps: with ps;
    [ jsonschema
      pytest
      requests
    ]);
in
  pkgs.mkShell {
    name = "python-env";

    buildInputs = [ 
        myPython
        pkgs.git
        pkgs.gnumake
    ];
  }