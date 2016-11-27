
set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

" Plugin 'vim-syntastic/syntastic'
Plugin 'Shougo/vimproc.vim'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/gitignore'
Plugin 'Valloric/YouCompleteMe'
Plugin 'vim-scripts/haskell.vim'
Plugin 'elzr/vim-json'
Plugin 'toyamarinyon/vim-swift'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'

" end plugin includes
call vundle#end()            " required
filetype plugin indent on    " required

set omnifunc=syntaxcomplete#Complete
" custom config below

if has("gui_macvim")
    set macligatures
    set guifont=Meslo LG M DZ Regular for Powerline:h13
endif

syntax enable
colorscheme lucius
LuciusDark
" LuciusDarkHighContrast

let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

" keybinds
nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>nc :bclose<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>nt :NERDTreeToggle<cr>

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

" nerdtree settings
let g:NERDTreeQuitOnOpen=1

" airline settings
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_buffers=1
set laststatus=2

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0

" neocomplete
let g:neocomplete#enable_at_startup=1

" haskell
let g:no_haskell_conceal=1
let g:haskell_conceal=0
let g:haskell_conceal_wide=0
let g:haskell_conceal_enumerations=0
let g:haskell_tabular=1

" ycm
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
