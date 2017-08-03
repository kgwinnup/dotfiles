
set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/reno/.vim/bundle/Vundle.vim
set rtp+=~/.local/bin
set rtp+=~/.vim/bundle/ocp-indent-vim
set shell=bash\ -i

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

Plugin 'vim-syntastic/syntastic'
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
Plugin 'Shougo/vimproc'
Plugin 'ervandew/supertab'
Plugin 'elzr/vim-json'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'lervag/vimtex'
Plugin 'nvie/vim-flake8'
Plugin 'let-def/ocp-indent-vim'
Plugin 'vim-scripts/cabal.vim'
Plugin 'vim-scripts/haskell.vim'
"Plugin 'alx741/vim-hindent'
Plugin 'eagletmt/ghcmod-vim'
"Plugin 'bitc/vim-hdevtools'
Plugin 'eagletmt/neco-ghc'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/neocomplete.vim'
Plugin 'Twinside/vim-hoogle'
Plugin 'nbouscal/vim-stylish-haskell'
Plugin 'majutsushi/tagbar'

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
"inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>

nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>ns <C-W><C-W>
nnoremap <leader>nb :TagbarOpenAutoClose<cr>
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
autocmd FileType python nnoremap <buffer><leader>rr :!clear && python %<cr>
autocmd FileType python nnoremap <buffer><leader>rt :!clear && pytest -s -v<cr>
autocmd FileType haskell nnoremap <buffer><leader>rr :!clear && stack run<cr>
autocmd FileType haskell nnoremap <buffer><leader>re :!clear && stack %<cr>
autocmd FileType haskell nnoremap <buffer><leader>rt :!clear && stack test<cr>
autocmd FileType haskell nnoremap <buffer><leader>rb :!clear && stack build<cr>
autocmd FileType haskell nnoremap <buffer><leader>rc :GhcModCheckAndLintAsync<cr>
autocmd FileType haskell nnoremap <buffer><leader>t :GhcModType<cr>
autocmd FileType haskell nnoremap <buffer><leader>i :GhcModInfo<cr>
autocmd FileType ocaml nnoremap <buffer><leader>t :MerlinTypeOf<cr>
autocmd FileType ocaml nnoremap <buffer><leader>l :Locate<cr>
autocmd FileType ocaml nnoremap <buffer><leader>rr :!clear && jbuilder build main.exe && ./main.exe<cr>
autocmd FileType ocaml nnoremap <buffer><leader>rb :!clear && jbuilder build main.exe<cr>

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

set completeopt=menuone,menu,longest

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
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }


let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=0
let g:syntastic_check_on_w=0
let g:syntastic_check_on_wq=0

" clang format
let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AllowShortIfStatementsOnASingleLine" : "true",
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11"}

" python
let g:ycm_python_binary_path = 'python'

" ycm
let g:ycm_global_ycm_extra_conf="~/.vim/.ycm_extra_conf.py"

" javascript/jsx
let g:jsx_ext_required = 0

" markdown
let g:vim_markdown_folding_disabled = 1

" yaml
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

" ocaml
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"


" haskell
" install hlint, hoogle, ghcmod
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd FileType haskell setlocal formatprg=hindent
autocmd FileType haskell setlocal shiftwidth=2
autocmd FileType haskell setlocal tabstop=2
"autocmd BufWritePost *.hs GhcModCheckAndLintAsync
let g:ghcmod_hlint_options = ['--ignore=Redundant $', '--ignore=Use camelCase']
let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

