{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc924;
          # packages = { 
            # You can add more than one local package here.
            # my-package.root = ./.;  # Assumes ./my-package.cabal
          # };
          # buildTools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          buildTools = hp: {
            inherit (pkgs)
              stack
              lambdabot;
            #   chromedriver
            #   chromium;
            inherit (hp)
              fourmolu;
          };
          # overrides = self: super: { };
          # hlintCheck.enable = true;
          # hlsCheck.enable = true;
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.mht2img;
      };
    };
}
