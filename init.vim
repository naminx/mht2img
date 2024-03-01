set nocompatible

syntax on

" set 'selection', 'selectmode', 'mousemodel' and 'keymodel' for MS-Windows
behave mswin

" backspace in Visual mode deletes selection
vnoremap <BS> d

" clipboard with win32yank.exe
" in ~/.local/bin/win32yank.exe
" let g:clipboard = {
"   \   'name': 'WslClipboard',
"   \   'copy': {
"   \     '+': '/mnt/c/Windows/System32/clip.exe',
"   \     '*': '/mnt/c/Windows/System32/clip.exe',
"   \    },
"   \   'paste': {
"   \     '+': '/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
"   \     '*': '/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
"   \   },
"   \   'cache_enabled': 0,
"   \ }
if (!empty($WAYLAND_DISPLAY) && executable("wl-copy") && executable("wl-paste"))
  let g:clipboard = {
    \   'name': 'wl-copy',
    \   'copy': {
    \     '+': 'wl-copy --type text/plain',
    \     '*': 'wl-copy --primary --type text/plain',
    \   },
    \   'paste': {
    \     '+': 'wl-paste --no-newline',
    \     '*': 'wl-paste --no-newline --primary',
    \   },
    \   'cache_enabled': 0,
    \ }
endif

if 1
  " CTRL-X and SHIFT-Del are Cut
  vnoremap <C-X> "+x
  vnoremap <S-Del> "+x

  " CTRL-C and CTRL-Insert are Copy
  vnoremap <C-C> "+y
  vnoremap <C-Insert> "+y

  " CTRL-V and SHIFT-Insert are Paste
  map <C-V> "+gP
  map <S-Insert> "+gP

  cmap <C-V> <C-R>+
  cmap <S-Insert> <C-R>+
endif

" Pasting blockwise and linewise selections is not possible in Insert and
" Visual mode without the +virtualedit feature.  They are pasted as if they
" were characterwise instead.
" Uses the paste.vim autoload script.
" Use CTRL-G u to have CTRL-Z only undo the paste.

if 1
  exe 'inoremap <script> <C-V> <C-G>u' . paste#paste_cmd['i']
  exe 'vnoremap <script> <C-V> ' . paste#paste_cmd['v']
endif

imap <S-Insert> <C-V>
vmap <S-Insert> <C-V>

" Use CTRL-Q to do what CTRL-V used to do
noremap <C-Q> <C-V>

" Use CTRL-S for saving, also in Insert mode (<C-O> doesn't wor k well when
" using completions).
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <Esc>:update<CR>gi

" For CTRL-V to work autoselect must be off.
" On Unix we have two selections, autoselect can be used.
if !has("unix")
  set guioptions-=a
endif

" CTRL-Z is Undo; not in cmdline though
noremap <C-Z> u
inoremap <C-Z> <C-O>u

" CTRL-Y is Redo (although not repeat); not in cmdline though
noremap <C-Y> <C-R>
inoremap <C-Y> <C-O><C-R>

" CTRL-A is Select all
noremap <C-A> gggH<C-O>G
inoremap <C-A> <C-O>gg<C-O>gH<C-O>G
cnoremap <C-A> <C-C>gggH<C-O>G
onoremap <C-A> <C-C>gggH<C-O>G
snoremap <C-A> <C-C>gggH<C-O>G
xnoremap <C-A> <C-C>ggVG

" Use tab for trigger completion with characters ahead and navigate
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function',''')}

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1,1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0,1) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1,1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0,1)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1,1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0,1) : "\<C-b>"
endif
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm() : "\<CR>"

set statusline=%r%h%w%=\ [%4l,%3v]%3p%%\ [%L]
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

let g:rainbow_active = 1

" when there are unwanted white spaces and tabs, highlight them
set listchars=tab:>~,nbsp:_,trail:.
exec "set listchars=tab:\uBB\uBB,trail:\uB7,nbsp:~"
set list

set termguicolors
set background=dark
colorscheme gruvbox

set number
set colorcolumn=80
let g:airline_section_z = '%#__accent_bold#%v%#__restore__#:%#__accent_bold#%l%#__restore__#/%L☰%#__accent_bold#%p%%%#__restore__#'

set incsearch hlsearch
