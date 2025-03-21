{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    taggy-lens.url = "github:alpmestan/taggy-lens?rev=87235bfb9c3ee8b3d487c1cf48a22f247e59286d";
    taggy-lens.flake = false;

    image-type.url = "github:naminx/image-type?rev=41fa3732ed55b65a1067377f9cc95606cc44f97c";
    image-type.flake = false;

    webp.url = "github:naminx/webp?rev=cd5b4a37f16b0205fdf8f6ab1040843a9cd394fc";
    webp.flake = false;
  };

  outputs = inputs @ {nixpkgs, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = {
        self',
        config,
        pkgs,
        ...
      }: {
        packages.default = self'.packages.mht2img;
        haskellProjects.default = {
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`, e.g. from Hackage
          packages = {
            # aeson.source = "1.5.0.0" # Hackage version
            taggy-lens.source = inputs.taggy-lens;
            image-type.source = inputs.image-type;
            webp.source = inputs.webp;
          };

          # my-haskell-package development shell configuration
          devShell = {
            hlsCheck.enable = true;
            hoogle = false;
            tools = hp: {
              inherit
                (hp)
                ;
            };
          };

          # What should haskell-flake add to flake outputs?
          autoWire = ["packages" "apps"]; # Wire all but the devShell
        };

        devShells.default = pkgs.mkShell {
          name = "haskell dev";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            # other development tools.
            # (import ./vim.nix {inherit pkgs;})
          ];
        };
      };
    };
}
