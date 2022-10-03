{
  description = "scanweb";

  nixConfig = {
    bash-prompt = "\n\\[\\033[0;94m\\][\\[\\e]0;\\u@\\h: \\w\\a\\]\\u@\\h:\\w]\$\\[\\033[0m\\] ";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";

    purebred-email = {
      url = "github:purebred-mua/purebred-email/7ba346e10ad1521a923bc04a4ffeca479d8dd071";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { pkgs, inputs', self', ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc924;
          root = ./.;
          name = "scanweb";
          buildTools = hp: {
            # inherit (pkgs)
            #   neovim;
            # inherit (hp)
            #   fourmolu;
          };
          source-overrides = {
            inherit (inputs)
              purebred-email;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            purebred-email = dontCheck (doJailbreak super.purebred-email);
          };
        };
      };
    };
}
