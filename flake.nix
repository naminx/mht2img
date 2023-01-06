{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc944;
          buildTools = hp: {
            inherit (pkgs)
              lambdabot
              stack
              zlib;
            inherit (hp)
              fourmolu;
          };
          overrides = self: super: {
            ghcid = pkgs.haskell.lib.dontCheck super.ghcid;
          };
        };
      };
    };
}
