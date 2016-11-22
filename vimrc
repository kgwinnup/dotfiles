
set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

Plugin 'jonathanfilip/vim-lucius'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree'
Plugin 'Shougo/neocomplete.vim'
Plugin 'vim-scripts/haskell.vim'
Plugin 'Shougo/neomru.vim'
Plugin 'Shougo/unite.vim' 
Plugin 'eagletmt/unite-haddock'
Plugin 'ujihisa/unite-haskellimport'
Plugin 'elzr/vim-json'
Plugin 'keith/swift.vim'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'

" end plugin includes
call vundle#end()            " required
filetype plugin indent on    " required

" custom config below

if has("gui_macvim")
    set macligatures
    set guifont=Meslo LG M DZ Regular for Powerline:h13
endif

syntax enable
colorscheme lucius
LuciusDarkHighContrast

let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

nnoremap <leader>bp :bprevious<cr>
nnoremap <leader>bn :bnext<cr>
nnoremap <leader>bd :bdelete<cr>
nnoremap <leader>nt :NERDTreeToggle<cr>

set ruler
set number
set autoindent           	" Copy indent from current line
set softtabstop=4
set tabstop=4
set shiftwidth=4
set autoread             	" Automatically reload changed files
set expandtab            	" Use spaces not tabs
set exrc                        " Read ./.vimrc
set fileformats=unix,dos,mac    " Choose line ending sanely
set shiftwidth=2	        " 2 space indent stops
set showmatch            	" Highlight matching brackets
set splitbelow           	" Put new splits down
set splitright           	" Put new vsplits right
set wildmenu             	" Show the completion menu when tab completing
set wildmode=list:longest,full  " Configure wildmenu

" nerdtree settings
let g:NERDTreeQuitOnOpen=1

" airline settings
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
