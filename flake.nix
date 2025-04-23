{
  description = "Quant analysis development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        
        # Rust toolchain
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" "clippy" ];
        };
        
        # Haskell toolchain
        haskellPackages = pkgs.haskellPackages;
        
        # Development shell packages
        devShellPackages = with pkgs; [
          # Rust
          rustToolchain
          
          # Haskell
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.stack
          haskellPackages.haskell-language-server
          
          # ArgoCD and K8s tools
          argocd
          kubectl
          kubernetes-helm
          
          # ClickHouse
          clickhouse-cli
          
          # HyperDX
          (pkgs.writeShellScriptBin "hyperdx" ''
            echo "Running HyperDX CLI wrapper - this is a placeholder"
            # In actual implementation, this might download or configure HyperDX
          '')
          
          # Dev tools
          git
          gnumake
          jq
        ];
      in
      {
        # Development shell with all tools
        devShells.default = pkgs.mkShell {
          buildInputs = devShellPackages;
          
          shellHook = ''
            echo "🚀 Welcome to the Quant Analysis Development Environment"
            echo "Available tools:"
            echo " - Rust: $(rustc --version)"
            echo " - GHC: $(ghc --version)"
            echo " - ArgoCD: $(argocd version --client 2>/dev/null || echo 'CLI only')"
            echo " - Helm: $(helm version --short)"
            echo " - ClickHouse CLI: $(clickhouse-client --version 2>/dev/null || echo 'Available')"
            echo ""
            echo "Happy coding! 📈"
          '';
          
          # Environment variables if needed
          # RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
        };

        # Also include packages if you want them available outside the devShell
        packages = {
          default = pkgs.symlinkJoin {
            name = "quant-analysis-tools";
            paths = devShellPackages;
          };
        };
      }
    );
}
