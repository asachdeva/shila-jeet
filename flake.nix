{
  description = "Shila Jeet's development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
    };
    # Add the pinecone repository as an input
    pinecone-src = {
      url = "github:MercuryTechnologies/pinecone";
      flake = false;
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
          extensions = [ "rust-src" "rust-analyzer" "clippy" "rustfmt" ];
        };
        
        # Haskell toolchain
        haskellPackages = pkgs.haskellPackages;

        # Haskell package set for streaming with pinecone
        projectHaskellPackages = haskellPackages.extend (self: super: {
          # Add pinecone from Mercury Technologies
          pinecone = self.callCabal2nix "pinecone" pinecone-src {};
        });

        
        # Development shell packages
        devShellPackages = with pkgs; [
          # Rust
          rustToolchain
          
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

          # Pinecone CLI wrapper
          (pkgs.writeShellScriptBin "pinecone-cli" ''
            echo "Running Pinecone CLI wrapper"
            echo "This is a convenience tool for interacting with Pinecone vector DB"
            echo "Usage: pinecone-cli [command]"
            echo "Commands:"
            echo "  list-indexes - List your Pinecone indexes"
            echo "  describe-index [name] - Describe a specific index"
            echo "  query [index] [vector] - Query vectors from an index"
            # Actual implementation would call the appropriate API endpoints
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
            author:             Akshay Sachdeva
            maintainer:         asachdeva@utexas.edu
            build-type:         Simple
            
            library
              exposed-modules:     shila-jeet
              build-depends:       base >= 4.14 && < 5
                                 , streamly ^>=0.9.0
                                 , streamly-core ^>=0.1.0
                                 , pinecone
                                 , aeson
                                 , text
                                 , vector
              hs-source-dirs:      src
              default-language:    Haskell2010
              ghc-options:         -Wall
            
            executable shila-jeet
              main-is:             Main.hs
              build-depends:       base >= 4.14 && < 5
                                 , shila-jeet
                                 , streamly ^>=0.9.0
                                 , streamly-core ^>=0.1.0
                                 , pinecone
                                 , aeson
                                 , text
                                 ,vector
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
            {-# LANGUAGE OverloadedStrings #-}

            module QuantAnalysis where

            import qualified Streamly.Data.Stream as Stream
            import qualified Streamly.Data.Fold as Fold
            import Pinecone
            import qualified Data.Vector as V
            import Date.Text (Text)
            import Date.Aeson (Object)

            -- | Process a stream of financial data
            processFinancialData :: Stream.Stream IO Double -> IO Double
            processFinancialData stream = Stream.fold Fold.sum stream

                      -- | Create a vector config for financial data analysis
            createVectorConfig :: Int -> Text -> VectorConfig
            createVectorConfig dimensions metricType =
              VectorConfig
                { vcDimension = dimensions
                , vcMetric = metricType
                , vcPods = 1
                , vcReplicas = 1
                , vcPodType = "s1.x1"
                }
            
            -- | Store financial data vectors in Pinecone
            storeFinancialVector :: Client -> Text -> V.Vector Float -> Maybe Object -> IO ()
            storeFinancialVector client namespace vectorData metadata = do
              let vector = Vector
                    { vId = "fin-" <> namespace
                    , vValues = vectorData
                    , vSparseValues = Nothing
                    , vMetadata = metadata
                    }
              _ <- upsert client namespace [vector]
              return ()
            
            -- | Query similar vectors from Pinecone
            queryFinancialVectors :: Client -> Text -> V.Vector Float -> Int -> IO [QueryMatch]
            queryFinancialVectors client namespace queryVector topK = do
              let queryReq = QueryRequest
                    { qVector = Just queryVector
                    , qSparseVector = Nothing
                    , qTopK = topK
                    , qFilter = Nothing
                    , qIncludeValues = True
                    , qIncludeMetadata = True
                    , qNamespace = namespace
                    }
              response <- query client queryReq
              return $ qmMatches response
          '';
          destination = "/src/QuantAnalysis.hs";
        };
       
             # Generate a basic Main file with Mercury's Pinecone usage
        mainHs = pkgs.writeTextFile {
          name = "Main.hs";
          text = ''
            {-# LANGUAGE OverloadedStrings #-}
            
            module Main where

            import qualified Streamly.Data.Stream as Stream
            import QuantAnalysis
            import Pinecone
            import qualified Data.Vector as V
            import System.Environment (getEnv)
            import Data.Text (Text)

            main :: IO ()
            main = do
              putStrLn "Quant Analysis Lambda Function with Pinecone Integration"
              
              -- Example data stream (in a real application, this would come from market data)
              let dataStream = Stream.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
              result <- processFinancialData dataStream
              putStrLn $ "Total: " ++ show result
              
              -- Initialize Pinecone client
              apiKey <- getEnv "PINECONE_API_KEY"
              let config = ClientConfig
                    { ccApiKey = apiKey
                    , ccEnvironment = "us-west1-gcp"  -- replace with your environment
                    , ccProjectId = "your-project-id" -- replace with your project ID
                    }
              
              client <- newClient config
              
              -- Check if our index exists, if not create it
              let indexName = "quant-analysis"
              indexes <- listIndexes client
              
              if not (indexName `elem` indexes)
                then do
                  putStrLn $ "Creating index: " ++ show indexName
                  let vectorConfig = createVectorConfig 5 "cosine"
                  _ <- createIndex client indexName vectorConfig
                  putStrLn "Index created successfully"
                else
                  putStrLn "Index already exists"
              
              -- Store example vector in Pinecone
              let namespace = "financial-metrics"
                  exampleVector = V.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
              
              storeFinancialVector client namespace exampleVector Nothing
              putStrLn "Vector stored in Pinecone"
              
              -- Query similar vectors
              matches <- queryFinancialVectors client namespace exampleVector 3
              putStrLn $ "Found " ++ show (length matches) ++ " similar vectors"
          '';
          destination = "/app/Main.hs";
        };


        # Basic Rust project with bartors dependency
        rustCargoToml = pkgs.writeTextFile {
          name = "Cargo.toml";
          text = ''
            [package]
            name = "shila-jeet-rust"
            version = "0.1.0"
            edition = "2021"

            [dependencies]
            barters = "0.1.6"  # Add the barters dependency
          '';
          destination = "/Cargo.toml";
        };

        rustSrcMain = pkgs.writeTextFile {
          name = "main.rs";
          text = ''
            // Example Rust code using barters
            use barters::prelude::*;

            fn main() {
                println!("Quant Analysis Rust Component");
                
                // Example bartors usage (placeholder)
                // This is just an example - replace with actual barters use based on your needs
                let input = vec![1.0, 2.0, 3.0, 4.0, 5.0];
                println!("Input data: {:?}", input);
                
                // TODO: Implement your trading strategy logic using barters
            }
          '';
          destination = "/src/main.rs";
        };
        
        # Create a script to set up the project structure
        setupProjectScript = pkgs.writeShellScriptBin "setup-project" ''
          # Set up Haskell project
          mkdir -p src app
          cp ${cabalProject}/cabal.project .
          cp ${cabalFile}/shila-jeet.cabal .
          cp ${quantAnalysisModule}/src/QuantAnalysis.hs src/
          cp ${mainHs}/app/Main.hs app/

          echo "Project structure set up with Streamly and Mercury's Pinecone integration!"
          echo "To build: cabal build"
          echo ""
          echo "Don't forget to set the PINECONE_API_KEY environment variable before running."

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
            projectHaskellPackages.pinecone
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
            echo " - Pinecone CLI: Available (pincecone-cli)"
            echo " - Mercury's Pinecone Integration:"
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
