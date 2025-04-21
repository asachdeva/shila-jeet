{ pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  buildInputs = with pkgs; [ curl morph];
  shellHook = "Welcome to the shell of Shila-Jeet";
}
