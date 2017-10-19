set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.local/bin

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

Plugin 'w0rp/ale' " asynchronous lint engine
Plugin 'jonathanfilip/vim-lucius'
Plugin 'morhetz/gruvbox'
Plugin 'jordwalke/flatlandia'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/gitignore'
Plugin 'Valloric/YouCompleteMe'
Plugin 'rdnetto/YCM-Generator'
Plugin 'rhysd/vim-clang-format'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'nbouscal/vim-stylish-haskell'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'Shougo/vimproc'
Plugin 'vim-scripts/cabal.vim'
Plugin 'ervandew/supertab'
Plugin 'elzr/vim-json'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'lervag/vimtex'
Plugin 'nvie/vim-flake8'

" end plugin includes
call vundle#end()            " required
filetype plugin indent on    " required

" set omnifunc=syntaxcomplete#Complete
" custom config below

if has("gui_macvim")
    set macligatures
    set guifont=Meslo\ LG\ M\ DZ\ Regular\ for\ Powerline:h13
endif

syntax enable
"set background=light
"colorscheme gruvbox
colorscheme lucius
LuciusDarkLowContrast
"colorscheme flatlandia

let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

" keybinds
nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>ns <C-W><C-W>
nnoremap <leader>+ :exe "resize " . (winheight(0) + 5)<cr>
nnoremap <leader>- :exe "resize " . (winheight(0) - 5)<cr>
nnoremap <leader>nt :NERDTreeToggle<cr>
nnoremap <leader>st :ALEToggle<cr>
nnoremap <leader>ym :YcmGenerateConfig<cr>
nnoremap <leader>ss :setlocal spell spelllang=en_us<cr>
nnoremap <leader>sf :setlocal nospell<cr>
inoremap jj <esc>
autocmd FileType c,cpp,ocaml nnoremap <buffer><leader>rr :!clear && make run<cr>
autocmd FileType c,cpp,ocaml nnoremap <buffer><leader>rt :!clear && make test<cr>
autocmd FileType c,cpp,ocaml nnoremap <buffer><leader>rb :!clear && make<cr>
autocmd FileType c,cpp,javascript nnoremap <buffer><leader>rf :ClangFormat<cr>
autocmd FileType haskell nnoremap <buffer><leader>rr :!clear && stack run<cr>
autocmd FileType haskell nnoremap <buffer><leader>rt :!clear && stack test<cr>
autocmd FileType haskell nnoremap <buffer><leader>rb :!clear && stack build<cr>
autocmd FileType haskell nnoremap <buffer><leader>rs :!clear && stack %<cr>
autocmd FileType haskell nnoremap <buffer><leader>t :GhcModType<cr>
autocmd FileType python nnoremap <buffer><leader>rr :!clear && python %<cr>
autocmd FileType python nnoremap <buffer><leader>rt :!clear && pytest<cr>
autocmd FileType ocaml nnoremap <buffer><leader>t :MerlinTypeOf<cr>

set ruler
set number
set autoindent              	" Copy indent from current line
set tabstop=4
set shiftwidth=4
set autoread                 	" Automatically reload changed files
set expandtab               	" Use spaces not tabs
set exrc                        " Read ./.vimrc
set fileformats=unix,dos,mac    " Choose line ending sanely
set showmatch                 	" Highlight matching brackets
set splitbelow                	" Put new splits down
set splitright                 	" Put new vsplits right
set wildmenu                	" Show the completion menu when tab completing
set wildmode=list:longest,full  " Configure wildmenu
set cursorline
set colorcolumn=110
set mouse=a
set backspace=indent,eol,start

" nerdtree settings
let g:NERDTreeQuitOnOpen=1

" airline settings
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_buffers=1
set laststatus=2

" ALE
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '>>'
let g:ale_sign_column_always = 1

" clang format
let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AllowShortIfStatementsOnASingleLine" : "true",
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11"}

" haskell
" stack install ghc-mod hlint
let g:no_haskell_conceal=1
let g:haskell_conceal=0
let g:haskell_conceal_wide=0
let g:haskell_conceal_enumerations=0
let g:haskell_tabular=1
au FileType haskell setl sw=2 sts=2 et

" python
let g:ycm_python_binary_path = 'python'

" ycm
let g:ycm_global_ycm_extra_conf="~/.vim/.ycm_extra_conf.py"
let g:ycm_server_python_interpreter="/Users/kgwinnup/reno/anaconda3/bin/python"

" javascript/jsx
let g:jsx_ext_required = 0

" markdown
let g:vim_markdown_folding_disabled = 1

