{
  description = "Montague — non-deterministic natural language parser (Haskell + Rust)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, crane }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Haskell — GHC 9.6 (upgrade from ghc8107; all extensions used are available)
        haskellPkgs = pkgs.haskell.packages.ghc96;
        montague-haskell = haskellPkgs.callCabal2nix "montague" ./. { };

        # Rust — via crane (caches dependencies separately from the build)
        craneLib = crane.mkLib pkgs;
        montague-rust = craneLib.buildPackage {
          src = craneLib.cleanCargoSource (craneLib.path ./.);
        };
      in
      {
        packages = {
          haskell = montague-haskell;
          rust = montague-rust;
          default = montague-haskell;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Haskell toolchain
            haskellPkgs.ghc
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server

            # Rust toolchain
            pkgs.cargo
            pkgs.rustc
            pkgs.rust-analyzer
            pkgs.clippy
            pkgs.rustfmt
          ];
        };
      }
    );
}
