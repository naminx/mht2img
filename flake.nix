{
  description = "mht2img";
  nixConfig = {
    bash-prompt = "\n\\[\\033[0;94m\\][\\[\\e]0;\\u@\\h: \\w\\a\\]\\u@\\h:\\w]\$\\[\\033[0m\\] ";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";

    purebred-email.url = "github:divipp/purebred-email/a036d07bcd2ec0acdd677b34b060c21210a397df";
    purebred-email.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { pkgs, inputs', self', ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc922;
          root = ./.;
          name = "mht2img";
          buildTools = hp: {
            # inherit (pkgs)
            #   chromium;
            inherit (hp)
              fourmolu;
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
