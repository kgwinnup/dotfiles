set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.local/bin

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

Plugin 'jonathanfilip/vim-lucius'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'flazz/vim-colorschemes'
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
Plugin 'rust-lang/rust.vim'
Plugin 'racer-rust/vim-racer'
Plugin 'artur-shaik/vim-javacomplete2'
Plugin 'roxma/nvim-completion-manager'
Plugin 'roxma/vim-hug-neovim-rpc'
Plugin 'gaalcaras/ncm-R'
Plugin 'jalvesaq/Nvim-R'

" end plugin includes
call vundle#end()            " required
filetype plugin indent on    " required

if has("gui_macvim")
    set macligatures
    set guifont=mononoki:h13
endif

" set omnifunc=syntaxcomplete#Complete
" custom config below

set t_Co=256
syntax enable
set background=dark
colorscheme gruvbox
"LuciusDarkLowContrast

let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

" keybinds
nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>ns <C-W><C-W>
nnoremap <leader>= <C-w>=
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
autocmd FileType haskell nnoremap <buffer><leader>re :!clear && stack %<cr>
autocmd FileType haskell nnoremap <buffer><leader>t :GhcModType<cr>
autocmd FileType python nnoremap <buffer><leader>rr :!clear && python %<cr>
autocmd FileType python nnoremap <buffer><leader>rt :!clear && pytest<cr>
autocmd FileType r nnoremap <buffer><leader>rr :call SendParagraphToR("silent", "down")<cr>
autocmd FileType r nnoremap <buffer><leader>rs :call StartR("R")<cr>
autocmd FileType r nnoremap <buffer><leader>rk :call StopR("R")<cr>
autocmd FileType r nnoremap <buffer><leader>rf :call SendFileToR("silent")<cr>

set ruler
set number
"set foldcolumn=2
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
let NERDTreeIgnore = ['\.pyc$', '^__pycache__$', '^node_modules']

" airline settings
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_buffers=1
set laststatus=2

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
let g:ycm_global_ycm_extra_conf = expand("~/.vim/.ycm_extra_conf.py")
let g:ycm_server_python_interpreter = expand("/usr/local/bin/python")

" javascript/jsx
let g:jsx_ext_required = 0

" markdown
let g:vim_markdown_folding_disabled = 1

" java
autocmd FileType java setlocal omnifunc=javacomplete#Complete


