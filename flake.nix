{
  description = "Mambda - a simple snake clone";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        ghc = "ghc912";
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
        haskellPkgs = pkgs.haskell.packages.${ghc}.override {
          overrides = ghcSelf: ghcSuper: {
            generic-optics = pkgs.haskell.lib.dontCheck (ghcSuper.generic-optics);
            mambda-cli = ghcSuper.callCabal2nix "mambda-cli" ./mambda-cli {};
          };
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            alejandra.enable = true;
            fourmolu.enable = true;
            just-fmt = {
              enable = true;
              name = "Just fmt";
              entry = "find -iname justfile -exec ${pkgs.just}/bin/just --fmt --unstable -f {} \;";
              files = "justfile";
              pass_filenames = false;
            };
          };
        };
      in {
        devShells.default = haskellPkgs.shellFor {
          packages = p: [haskellPkgs.mambda-cli];
          buildInputs =
            (with pkgs; [just])
            ++ (with haskellPkgs; [
              cabal-install
              haskell-language-server
              hpack
            ]);

          withHoogle = false;
          shellHook = ''
            ${pre-commit.shellHook}
          '';
        };
      }
    );
}
