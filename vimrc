"""" start vundle required
set nocompatible              " be iMproved
set shell=/bin/bash
filetype off                  "required by vundle

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Plugin 'gmarik/vundle'
Plugin 'Lokaltog/powerline'
Plugin 'bling/vim-airline'          "https://github.com/bling/vim-airline
Plugin 'edkolev/promptline.vim'     "https://github.com/edkolev/promptline.vim
Plugin 'Shougo/neocomplcache.vim'   "https://github.com/Shougo/neocomplcache.vim
Plugin 'ervandew/supertab'          "https://github.com/ervandew/supertab 
Plugin 'scrooloose/syntastic'       "https://github.com/scrooloose/syntastic
" Plugin 'majutsushi/tagbar'          "https://github.com/majutsushi/tagbar
" Plugin 'Lokaltog/vim-easymotion'    "https://github.com/Lokaltog/vim-easymotion
Plugin 'scrooloose/nerdtree'        "https://github.com/scrooloose/nerdtree
Plugin 'Shougo/unite.vim'           "https://github.com/Shougo/unite.vim
" Plugin 'tpope/vim-surround'         "https://github.com/tpope/vim-surround
" Plugin 'vim-pandoc/vim-pandoc'      "https://github.com/vim-pandoc/vim-pandoc
Plugin 'tpope/vim-markdown'         "https://github.com/tpope/vim-markdown
Plugin 'derekwyatt/vim-scala'       "https://github.com/derekwyatt/vim-scala

filetype plugin indent on     " required

"""" end vundle required

set ruler "show line and column number
syntax on
colorscheme zenburn
set history=1000
set nocompatible
set autoindent
set smartindent
set wildmenu
set softtabstop=4
set tabstop=4
set expandtab
set shiftwidth=4
filetype indent on
filetype plugin on
set incsearch
set number
set ignorecase
set smartcase
set tabpagemax=30
set fileformat=unix

if has("gui_running")
  if has("gui_gtk2")
    set guifont=Inconsolata\ 12
  elseif has("gui_macvim")
    set guifont=Ubuntu\ Mono\ derivative\ Powerline:h13
  elseif has("gui_win32")
    set guifont=Consolas:h11:cANSI
  endif
endif

""""""""""""""""""""""""""
" keybindings
""""""""""""""""""""""""""
map <F7> :tabp<CR>
map <F9> :tabn<CR>
nmap q <Nop>
nmap Z <Nop>

let g:Powerline_symbols = 'unicode'
set encoding=utf-8

"highliglight
syntax match nonascii "[^\x00-\x7F]"
highlight nonascii guibg=Red ctermbg=2

set visualbell

"incantations for intelligent 80 width coding and text
set tw=72 fo+=q wm=0
set fo-=t fo-=a fo-=c

"detect trailing whitespace
match Todo /\s\+$/

" Always show statusline
set laststatus=2

" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256
set encoding=utf-8
"set fillchars+=stl:\ ,stlnc:\

"nerdtree
map <Leader>nt :NERDTree<CR>

"omnicompletes
highlight PmenuSel ctermbg=2 "selected element given green background
" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

"neocomplcache
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Enable omni completion.
highlight PmenuSel ctermbg=2 "selected element given green background
autocmd FileType haskell  setlocal omnifunc=necoghc#omnifunc
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType c set omnifunc=ccomplete#Complete

"http://stackoverflow.com/questions/158968/changing-vim-indentation-behavior-by-file-type
autocmd FileType proto setlocal shiftwidth=2 tabstop=2
autocmd FileType scala setlocal shiftwidth=2 tabstop=2


""""""""""""""""""""""
" VIM-AIRLINE 
""""""""""""""""""""""
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

""""""""""""""""""""""
" AUTO COMPLETE STUFF
""""""""""""""""""""""

"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Enable heavy features.
" Use camel case completion.
"let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
"let g:neocomplcache_enable_underbar_completion = 1

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplcache#smart_close_popup() . "\<CR>"
endfunction

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplcache_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplcache_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

" powerline fonts
let g:Powerline_symbols = 'unicode'
let g:promptline_theme = 'airline'

let g:airline_theme = 'powerlineish'

let g:syntastic_quite_messages = { "type": "style" }

