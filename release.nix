let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./pong-wars.nix { }
