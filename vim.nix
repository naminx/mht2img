{pkgs}:
with pkgs; {
  customRC = lib.strings.fileContents ./init.vim;
  packages.myPlugins = with vimPlugins; {
    start = [
      coc-nvim # for haskell language server
      coc-python
      coc-prettier
      coc-json
      vim-fish # fish syntax highlighting
      gruvbox # color scheme close to "Groovy Lambda"
      haskell-vim # haskell syntax highlighting
      rainbow # color parenthesis
      vim-airline # customized status line
      vim-lastplace # remember last position
      vim-nix # nix source file highlight
      vim-ormolu # format haskell source file when saving
      # vim-prettier
    ];
    opt = [];
  };
}
