{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.simpleFlake  {
      inherit self nixpkgs;

      systems = [ "x86_64-linux"]; # Add more if needed

      devShells.default = nixpkgs.lib.mkShell {
        name = "development";
        packages = with nixpkgs; [
          haskell.ghc
          rustc
          cargo
          fourmolu
          rustfmt
          clickhouse-client
          helm
          hyperdx-cli
          argocd
        ];
        # You can add shellHook for environment setup if needed
        shellHook = ''
          echo "Welcome to the development shell for shila-jeet "
        '';
      };

      packages = rec {
        fourmolu-bin = pkgs.haskellPackages.fourmolu;
        rustfmt-bin = pkgs.rustPlatform.rustfmt;
        clickhouse-client-bin = pkgs.clickhouse-client;
        helm-bin = pkgs.kubernetes-helm;
        hyperdx-cli-bin = pkgs.hyperdx-cli;
        argocd-bin = pkgs.argocd;

        default = pkgs.mkShell {
          name = "tools";
          packages = [
            fourmolu-bin
            rustfmt-bin
            clickhouse-client-bin
            helm-bin
            hyperdx-cli-bin
            argocd-bin
          ];
        };

        pkgs = import nixpkgs { system = self.system; };
      };
    };
}
