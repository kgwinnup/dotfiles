set shortmess=at
set nocompatible              " be iMproved, required
filetype off                  " required

" init vundle
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.local/bin

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" start plugin includes

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'morhetz/gruvbox'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/gitignore'
Plugin 'rhysd/vim-clang-format'
Plugin 'ervandew/supertab'
Plugin 'elzr/vim-json'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'MaxMEllon/vim-jsx-pretty'
Plugin 'nvie/vim-flake8'
Plugin 'fatih/vim-go'
Plugin 'rust-lang/rust.vim'
Plugin 'jalvesaq/Nvim-R'
Plugin 'benmills/vimux'

" end plugin includes
call vundle#end()            " required
filetype plugin indent on    " required

if has("gui_macvim")
    set macligatures
    set guifont=mononoki:h13
endif

" custom config below
"
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
set clipboard+=unnamedplus

" nerdtree settings
let g:NERDTreeQuitOnOpen=1
let NERDTreeIgnore = ['\.pyc$', '^__pycache__$', '^node_modules']
let NERDTreeShowHidden=1

" airline settings
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_buffers=1
set laststatus=2

" system python
let g:python2_host_prog = $HOME + '/anaconda3/bin/python'
let g:python3_host_prog = $HOME + '/anaconda3/bin/python3'

set t_Co=256
syntax enable
set background=dark
colorscheme gruvbox

let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

let g:jsx_ext_required = 0

" global keybinds
nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>ns <C-W><C-W>
nnoremap <leader>j <C-d>
nnoremap <leader>k <C-u>
nnoremap <leader>= <C-w>=
nnoremap <leader>nt :NERDTreeToggle<cr>
nnoremap <leader>ss :setlocal spell spelllang=en_us<cr>
nnoremap <leader>sf :setlocal nospell<cr>
nnoremap <leader>sn ]s
nnoremap <leader>sp [s
nnoremap <leader>sr z=
nnoremap <leader>sa zg
nnoremap <leader>vp :VimuxPromptCommand<cr>
nnoremap <leader>vl :VimuxRunLastCommand<cr>
nnoremap <leader>mb :VimuxRunCommand('git blame -L ' . line('.') . ',' . line('.') . ' ' . expand('%:p'))<cr><cr>
nnoremap <leader>ml :VimuxRunCommand('git blame -L ' . line('.') . ',' . line('.') . ' ' . expand('%:p') . "\| awk '{print $1}' \| xargs git --no-pager log -n 1 --decorate")<cr><cr>
nnoremap <leader>mL :VimuxRunCommand('git blame -L ' . line('.') . ',' . line('.') . ' ' . expand('%:p') . "\| awk '{print $1}' \| xargs git --no-pager log -p -n 1 --decorate")<cr><cr>
autocmd FileType c,cpp,javascript nnoremap <buffer><leader>rf :ClangFormat<cr>
autocmd BufNewFile,BufRead *.rmd set syntax=r

com! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"
com! FormatJSON :%!python -m json.tool
nnoremap = :FormatXML<Cr>
nnoremap = :FormatJSON<Cr>

"
" markdown
"
augroup ft_markdown
autocmd FileType markdown nnoremap <buffer><leader>go gx<cr>
augroup END

"
" C
"
augroup ft_c
autocmd FileType c,cpp ClangFormatAutoEnable
let g:SuperTabDefaultCompletionType = "<c-n>"
let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AllowShortIfStatementsOnASingleLine" : "true",
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11"}
autocmd FileType c,cpp nnoremap <buffer><leader>t g]
autocmd FileType c,cpp nnoremap <buffer><leader>gg g] 1<cr><cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gp :pop<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gu :!ctags -R *<cr><cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gl :cs find s <cword><cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gc :cs add cscope.out<cr>
augroup END

"
" Rust
"
augroup ft_rust
let g:rustfmt_autosave = 1
let g:SuperTabDefaultCompletionType = "<c-n>"
autocmd FileType rust nnoremap <buffer><leader>t g]
autocmd FileType rust nnoremap <buffer><leader>gg g] 1<cr><cr>
autocmd FileType rust nnoremap <buffer><leader>gp :pop<cr>
autocmd FileType rust nnoremap <buffer><leader>gu :!ctags -R *<cr><cr>
augroup END

"
" Go
"
augroup ft_go
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
autocmd FileType go nnoremap <buffer><leader>t :GoInfo<cr>
autocmd FileType go nnoremap <buffer><leader>gg :GoDef<cr>
autocmd FileType go nnoremap <buffer><leader>gp <C-o><cr>
let g:go_fmt_command = "goimports"
augroup END


"
" R 
"
augroup ft_r
function SendToR ()
    let startline = line(".")
    let save_cursor = getpos(".")
    let line = SanitizeRLine(getline("."))
    let i = line(".")
    while i > 0 && line !~ "function"
        let i -= 1
    endwhile

    if i == 0
        let out = SendParagraphToR("silent", "down")
    else
        let out = SendFunctionToR("silent", "down")
    endif
endfunction

let R_assign = 0
let R_in_buffer = 0
let R_applescript = 1

autocmd FileType r nnoremap <buffer><leader>rr :call SendToR()<cr>
autocmd FileType r nnoremap <buffer><leader>rs :call StartR("R")<cr>
autocmd FileType r nnoremap <buffer><leader>rk :call StopR("R")<cr>
autocmd FileType r nnoremap <buffer><leader>rf :call SendParagraphToR("silent", "down")<cr>
augroup END

