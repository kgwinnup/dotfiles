set shortmess=at

set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.local/bin

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

Plugin 'Lenovsky/nuake'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'flazz/vim-colorschemes'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/gitignore'
Plugin 'rhysd/vim-clang-format'
Plugin 'ervandew/supertab'
Plugin 'elzr/vim-json'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'lervag/vimtex'
Plugin 'nvie/vim-flake8'
Plugin 'fatih/vim-go'
Plugin 'mdempsky/gocode', {'rtp': 'vim/'}
Plugin 'jalvesaq/Nvim-R'
Plugin 'neovimhaskell/haskell-vim.git'
Plugin 'nbouscal/vim-stylish-haskell'

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

let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

" keybinds
tnoremap <Esc> <C-\><C-n>
nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>ns <C-W><C-W>
nnoremap <leader>= <C-w>=
nnoremap <leader>nt :NERDTreeToggle<cr>
nnoremap <leader>st :ALEToggle<cr>
nnoremap <leader>ss :setlocal spell spelllang=en_us<cr>
nnoremap <leader>sf :setlocal nospell<cr>
nnoremap <leader>tt :Nuake<cr>
tnoremap <leader>tt <C-\><C-n>:Nuake<CR>
inoremap jj <esc>
autocmd FileType c,cpp nnoremap <buffer><leader>rr :!clear && make run<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>rt :!clear && make test<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>rb :!clear && make<cr>
autocmd FileType c,cpp,javascript nnoremap <buffer><leader>rf :ClangFormat<cr>
autocmd FileType python nnoremap <buffer><leader>rr :!clear && python %<cr>
autocmd FileType python nnoremap <buffer><leader>rt :!clear && python -m pytest -s %<cr>
autocmd FileType go nnoremap <buffer><leader>rb :!clear && go build<cr>
autocmd FileType go nnoremap <buffer><leader>rt :!clear && go test<cr>
autocmd FileType go nnoremap <buffer><leader>t :GoInfo<cr>
autocmd FileType r nnoremap <buffer><leader>rr :call SendParagraphToR("silent", "down")<cr>
autocmd FileType r nnoremap <buffer><leader>rs :call StartR("R")<cr>
autocmd FileType r nnoremap <buffer><leader>rk :call StopR("R")<cr>
autocmd FileType r nnoremap <buffer><leader>rf :call SendFileToR("silent")<cr>
autocmd FileType haskell nnoremap <buffer><leader>rr :!clear && stack %<cr>
autocmd FileType haskell nnoremap <buffer><leader>rb :!clear && stack build<cr>
autocmd FileType haskell nnoremap <buffer><leader>rt :!clear && stack test<cr>

com! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"
com! FormatJSON :%!python3 -m json.tool
nnoremap = :FormatXML<Cr>
nnoremap = :FormatJSON<Cr>

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

" supertab
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"

" javascript/jsx
let g:jsx_ext_required = 0

" markdown
let g:vim_markdown_folding_disabled = 1

" R
let R_assign = 0
let R_in_buffer = 0
let R_applescript = 1

" Haskell
au FileType haskell setl sw=2 sts=2 et


