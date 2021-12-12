let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-21.05") { };
in
pkgs.mkShell {
  buildInputs = [
    # ELM
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-language-server

    # NIX
    pkgs.nixpkgs-fmt
  ];
}
