{
  description = "Shila Jeet's development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
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
        
        # Haskell package set for streaming
        projectHaskellPackages = haskellPackages.extend (self: super: {});

        
        # Development shell packages
        devShellPackages = with pkgs; [
          # Rust
          rustToolchain
          rustFmt
          
          # Haskell
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.stack
          haskellPackages.haskell-language-server
          haskellPackages.fourmolu
          
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

        # Generate a cabal.project file for the Streamly dependency
        cabalProject = pkgs.writeTextFile {
          name = "cabal.project";
          text = ''
            packages: ./shila-jeet.cabal
            
            package streamly
              flags: +use-atomic-primops
          '';
          destination = "/cabal.project";
        };
        
         # Generate a basic cabal file for the project
        cabalFile = pkgs.writeTextFile {
          name = "shila-jeet.cabal";
          text = ''
            cabal-version:      3.0
            name:               shila-jeet
            version:            0.1.0.0
            license:            BSD-3-Clause
            author:             Quant Developer
            maintainer:         developer@example.com
            build-type:         Simple
            
            library
              exposed-modules:     QuantAnalysis
              build-depends:       base >= 4.14 && < 5
                                 , streamly ^>=0.9.0
                                 , streamly-core ^>=0.1.0
              hs-source-dirs:      src
              default-language:    Haskell2010
              ghc-options:         -Wall
            
            executable shila-jeet
              main-is:             Main.hs
              build-depends:       base >= 4.14 && < 5
                                 , shila-jeet
                                 , streamly ^>=0.9.0
                                 , streamly-core ^>=0.1.0
              hs-source-dirs:      app
              default-language:    Haskell2010
              ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N
          '';
          destination = "/shila-jeet.cabal";
        };

          # Generate a basic module file
        quantAnalysisModule = pkgs.writeTextFile {
          name = "QuantAnalysis.hs";
          text = ''
            module QuantAnalysis where

            import qualified Streamly.Data.Stream as Stream
            import qualified Streamly.Data.Fold as Fold

            -- | Process a stream of financial data
            processFinancialData :: Stream.Stream IO Double -> IO Double
            processFinancialData stream = Stream.fold Fold.sum stream
          '';
          destination = "/src/QuantAnalysis.hs";
        };
        
        # Generate a basic Main file
        mainHs = pkgs.writeTextFile {
          name = "Main.hs";
          text = ''
            module Main where

            import qualified Streamly.Data.Stream as Stream
            import QuantAnalysis (processFinancialData)

            main :: IO ()
            main = do
              putStrLn "Quant Analysis Lambda Function"
              -- Example data stream (in a real application, this would come from market data)
              let dataStream = Stream.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
              result <- processFinancialData dataStream
              putStrLn $ "Total: " ++ show result
          '';
          destination = "/app/Main.hs";
        };

        # Basic Rust project with bartors dependency
        rustCargoToml = pkgs.writeTextFile {
          name = "Cargo.toml";
          text = ''
            [package]
            name = "quant-analysis-rust"
            version = "0.1.0"
            edition = "2021"

            [dependencies]
            bartors = "0.1.6"  # Add the bartors dependency
          '';
          destination = "/Cargo.toml";
        };

        rustSrcMain = pkgs.writeTextFile {
          name = "main.rs";
          text = ''
            // Example Rust code using bartors
            use bartors::prelude::*;

            fn main() {
                println!("Quant Analysis Rust Component");
                
                // Example bartors usage (placeholder)
                // This is just an example - replace with actual bartors use based on your needs
                let input = vec![1.0, 2.0, 3.0, 4.0, 5.0];
                println!("Input data: {:?}", input);
                
                // TODO: Implement your trading strategy logic using bartors
            }
          '';
          destination = "/src/main.rs";
        };
        
        # Create a script to set up the project structure
        setupProjectScript = pkgs.writeShellScriptBin "setup-project" ''
          # Set up Haskell project
          mkdir -p src app
          cp ${cabalProject}/cabal.project .
          cp ${cabalFile}/quant-analysis.cabal .
          cp ${quantAnalysisModule}/src/QuantAnalysis.hs src/
          cp ${mainHs}/app/Main.hs app/

          # Set up Rust project
          mkdir -p rust/src
          cp ${rustCargoToml}/Cargo.toml rust/
          cp ${rustSrcMain}/src/main.rs rust/src/
          
          echo "Project structure set up with Streamly and bartors dependencies!"
          echo "- Haskell project in current directory"
          echo "- Rust project in ./rust directory"
          echo ""
          echo "To build Haskell: cabal build"
          echo "To build Rust: cd rust && cargo build"
        '';

      in
      {
        # Development shell with all tools
        devShells.default = pkgs.mkShell {
          buildInputs = devShellPackages ++ [
            setupProjectScript
            projectHaskellPackages.streamly
            projectHaskellPackages.streamly-core
          ];

          
          shellHook = ''
            echo "🚀 Welcome to the Shila-Jeet's Development Environment"
            echo "Available tools:"
            echo " - Rust: $(rustc --version)"
            echo " - GHC: $(ghc --version)"
            echo " - Fourmolu: $(fourmolu --version)"
            echo " - rustfmt: $(rustfmt --version)"
            echo " - Haskell Language Server: $(haskell-language-server --version)"
            echo " - ArgoCD: $(argocd version --client 2>/dev/null || echo 'CLI only')"
            echo " - Helm: $(helm version --short)"
            echo " - ClickHouse CLI: $(clickhouse-client --version 2>/dev/null || echo 'Available')"
            echo ""
            echo "Happy coding! 📈"
          '';
          
          # Environment variables if needed
           RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
        };

        # Also include packages if you want them available outside the devShell
        packages = {
          default = pkgs.symlinkJoin {
            name = "shila-jeet-tools";
            paths = devShellPackages;
          };
        };
      }
    );
}
