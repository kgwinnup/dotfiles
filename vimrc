
set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.local/bin
set rtp+=~/.cargo/bin
set rtp+=~/.cargo/bin/rustc

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

Plugin 'vim-syntastic/syntastic'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/gitignore'
Plugin 'Valloric/YouCompleteMe'
Plugin 'rdnetto/YCM-Generator'
Plugin 'rhysd/vim-clang-format'
Plugin 'vim-scripts/haskell.vim'
Plugin 'Shougo/vimproc'
Plugin 'vim-scripts/cabal.vim'
Plugin 'ervandew/supertab'
Plugin 'elzr/vim-json'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'lervag/vimtex'
Plugin 'rust-lang/rust.vim'
Plugin 'cespare/vim-toml'
Plugin 'jalvesaq/Nvim-R'
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
colorscheme lucius
" LuciusDark
"LuciusDarkHighContrast
LuciusDarkLowContrast

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
nnoremap <leader>st :SyntasticToggleMode<cr>
nnoremap <leader>ym :YcmGenerateConfig<cr>
inoremap jj <esc>
autocmd FileType c,cpp nnoremap <buffer><leader>rr :!clear && make run<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>rt :!clear && make test<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>rb :!clear && make<cr>
autocmd FileType c,cpp,javascript nnoremap <buffer><leader>rf :ClangFormat<cr>
autocmd FileType haskell nnoremap <buffer><leader>rr :!clear && stack run<cr>
autocmd FileType haskell nnoremap <buffer><leader>rt :!clear && stack test<cr>
autocmd FileType haskell nnoremap <buffer><leader>rb :!clear && stack build<cr>
autocmd FileType rust nnoremap <buffer><leader>rr :!clear && cargo run<cr>
autocmd FileType rust nnoremap <buffer><leader>rb :!clear && cargo build<cr>
autocmd FileType rust nnoremap <buffer><leader>rt :!clear && cargo test<cr>
autocmd FileType rust nnoremap <buffer><leader>rf :RustFmt<cr>

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

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0

" clang format
let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AllowShortIfStatementsOnASingleLine" : "true",
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11"}

" haskell
let g:no_haskell_conceal=1
let g:haskell_conceal=0
let g:haskell_conceal_wide=0
let g:haskell_conceal_enumerations=0
let g:haskell_tabular=1
au FileType haskell setl sw=2 sts=2 et

" rust
let g:ycm_rust_src_path='/usr/local/rust/src'
let g:rustfmt_autosave=0
autocmd FileType rust let g:syntastic_rust_checkers=['rustc']
set hidden

" python
let g:ycm_python_binary_path = 'python'

" ycm
let g:ycm_global_ycm_extra_conf="~/.vim/.ycm_extra_conf.py"

