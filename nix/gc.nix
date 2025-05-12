{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [ pkgs.nix ];
  shellHook = ''
    nix-collect-garbage --delete-older-than 10d
  '';
}
