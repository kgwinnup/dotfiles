
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'morhetz/gruvbox'
Plug 'scrooloose/nerdtree'
Plug 'benmills/vimux'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'ludovicchabant/vim-gutentags'
" Plug 'dense-analysis/ale'
" Plug 'rust-lang/rust.vim'
" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'
" Plug 'vim-scripts/gitignore'
" Plug 'elzr/vim-json'
" Plug 'godlygeek/tabular'
" Plug 'plasticboy/vim-markdown'
" Plug 'MaxMEllon/vim-jsx-pretty'
" Plug 'nvie/vim-flake8'
" Plug 'jalvesaq/Nvim-R', {'branch': 'stable'}
call plug#end()

filetype off
syntax on
filetype plugin indent on
colorscheme gruvbox

set nocompatible
set modelines=0
set number
set ruler
set encoding=utf-8
set wrap
set textwidth=79
set tabstop=4
set softtabstop=4
set expandtab
set noshiftround
set hidden
set ttyfast
set laststatus=2
set showmode
set showcmd
set t_Co=256
set background=dark
set autoindent
set mouse=a
set backspace=indent,eol,start
set clipboard=unnamed,autoselect
" Show the completion menu when tab completing
set wildmenu
" Configure wildmenu
set wildmode=list:longest,full
" disable scratch/preview pane
set completeopt-=preview
" while the completion menu is open, map enter key to C-Y instead of return,
" this will select the item without adding a new line
inoremap <expr> <CR> pumvisible() ? "\<C-Y>" : "\<CR>"

" vimux
let g:VimuxOrientation = "h"

" nerdtree settings
let g:NERDTreeQuitOnOpen=1
let NERDTreeIgnore = ['\.pyc$', '^__pycache__$', '^node_modules']
let NERDTreeShowHidden=1

" leader key
let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

" global keybinds
nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>ns <C-W><C-W>
nnoremap <leader>j <C-d>
nnoremap <leader>k <C-u>
nnoremap <leader>= <C-w>=
nnoremap <leader>- :res -5<cr>
nnoremap <leader>+ :res +5<cr>
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

" supertab
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"

" Go
augroup ft_go
let g:gutentags_enabled = 0
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_functions_calls = 1
autocmd FileType go nnoremap <buffer><leader>t :GoInfo<cr>
autocmd FileType go nnoremap <buffer><leader>gg :GoDef<cr>
autocmd FileType go nnoremap <buffer><leader>gr :GoRename<cr>
autocmd FileType go nnoremap <buffer><leader>gl :GoReferrers<cr>
autocmd FileType go nnoremap <buffer><leader>gc :GoCallees<cr>
autocmd FileType go nnoremap <buffer><leader>gp <C-o><cr>
augroup END

" C
augroup ft_c
let g:gutentags_enabled = 1

if filereadable("cscope.out")
    cs add cscope.out
endif

function! MyClangFormat()
    let l:current_pos = getpos('.')
    execute '%!clang-format'
    call setpos('.', l:current_pos)
endfunction

set omnifunc=syntaxcomplete#Complete

" on file save, re-run the ctags command silently and in the background
autocmd BufWritePost *.c,*.h,*.cpp,*.hpp silent! !cscope -Rbq
autocmd BufWritePre *.c,*.h,*.cpp,*.hpp call MyClangFormat()
" autocmd FileType c,cpp ClangFormatAutoEnable
autocmd FileType c,cpp nnoremap <buffer><leader>gg <C-]>
autocmd FileType c,cpp nnoremap <buffer><leader>gp :pop<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gl :cs find s <cword><cr>
augroup END

" R
augroup ft_r
function! MySendRBlock()
    
    call VimuxSendKeys('Enter')
    let cur = getline('.')

    if stridx(cur, 'function') != -1 || stridx(cur, 'list(') != -1
        normal! V
        normal! ][
        normal! y
            
        let lines = split(@", "\n")
        for line in lines
            if len(line) > 0
                call VimuxTmux('send-keys -t '. g:VimuxRunnerIndex . ' ' . '"' . escape(line, '"') . '"')
                call VimuxSendKeys('Enter')
            else
                call VimuxSendKeys('Enter')
            endif
        endfor

        normal! ][
        normal! j
    else
        normal! V
        normal! }
        normal! y

        let lines = split(@", "\n")
        for line in lines
            if len(line) > 0
                call VimuxTmux('send-keys -t '. g:VimuxRunnerIndex . ' ' . '"' . escape(line, '"') . '"')
                call VimuxSendKeys('Enter')
            else
                call VimuxSendKeys('Enter')
            endif
        endfor

        normal! }
        normal! j
    endif
endfunction

autocmd FileType r,rmd nnoremap <buffer><leader>rr :call MySendRBlock()<cr>
augroup END
