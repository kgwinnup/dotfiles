
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'
Plug 'scrooloose/nerdtree'
Plug 'benmills/vimux'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'davidhalter/jedi-vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dense-analysis/ale'
call plug#end()

filetype off
syntax on
filetype plugin indent on
" colorscheme gruvbox
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
" always show the column left of the line numbers so lsp errors/warnings don't
" constantly shift the width
" set scl=yes
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

call system('mkdir -p ~/.vim/backups')
set backupdir=~/.vim/backups
set dir=~/.vim/backups/

" airline settings
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_buffers=1
set laststatus=2

"
" Ctrlp
"
let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_by_filename = 1

"
" vimux
" 
let g:VimuxOrientation = "h"

"
" nerdtree settings
"
let g:NERDTreeQuitOnOpen=1
let NERDTreeIgnore = ['\.pyc$', '^__pycache__$', '^node_modules']
let NERDTreeShowHidden=1

"
" leader key
"
let mapleader=" "
let g:mapleader=" "
set timeoutlen=2000

"
" global keybinds
"
nnoremap <leader>np :bprevious<cr>
nnoremap <leader>nn :bnext<cr>
nnoremap <leader>nd :bdelete<cr>
nnoremap <leader>no :only<cr>
nnoremap <leader>ns <C-W><C-W>
nnoremap <leader>nf :CtrlP<cr>
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

"
" supertab
"
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"

"
" Ale
"
let g:ale_linters_explicit = 1
let g:ale_sign_column_always = 0
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 1
let g:ale_warn_about_trailing_whitespace = 0
let g:ale_linters = {
\   'ocaml': ['merlin']
\}  
let g:ale_fixers = {
\   'r': ['R', '--slave', '-e', 'langaugeserver::run()'],
\   'c': ['clangd'],
\   'cpp': ['clangd'],
\   'ocaml': ['ocamlformat']
\}


"
" Go
"
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_functions_calls = 1
autocmd FileType go nnoremap <buffer><leader>t :GoInfo<cr>
autocmd FileType go nnoremap <buffer><leader>gg :GoDef<cr>
autocmd FileType go nnoremap <buffer><leader>gr :GoRename<cr>
autocmd FileType go nnoremap <buffer><leader>gl :GoReferrers<cr>
autocmd FileType go nnoremap <buffer><leader>gc :GoCallees<cr>
autocmd FileType go nnoremap <buffer><leader>gp <C-o><cr>

"
" C
"
function! MyClangFormat()
    let l:current_pos = getpos('.')
    execute '%!clang-format'
    call setpos('.', l:current_pos)
endfunction

set omnifunc=syntaxcomplete#Complete

autocmd BufWritePre *.c,*.h,*.cpp,*.hpp call MyClangFormat()
" autocmd FileType c,cpp ClangFormatAutoEnable
autocmd FileType c,cpp nnoremap <buffer><leader>t :ALEHover<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gg :ALEGoToDefinition<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gp :pop<cr>
autocmd FileType c,cpp nnoremap <buffer><leader>gl :ALEFindReferences<cr>

"
" Ocaml
"
autocmd FileType ocaml nnoremap <buffer><leader>t :ALEHover<cr>
autocmd FileType ocaml nnoremap <buffer><leader>gg :ALEGoToDefinition<cr>
autocmd FileType ocaml nnoremap <buffer><leader>gp :pop<cr>
autocmd FileType ocaml nnoremap <buffer><leader>gl :ALEFindReferences<cr>



"
" Python
"
let g:jedi#goto_command = ""
let g:jedi#goto_assignments_command = ""
let g:jedi#goto_stubs_command = ""
let g:jedi#goto_definitions_command = ""
let g:jedi#documentation_command = ""
let g:jedi#usages_command = ""
let g:jedi#completions_command = ""
let g:jedi#rename_command = ""
let g:jedi#rename_command_keep_name = ""

let g:vimux_temp_file = tempname()

function! MySendPyBlock()
    let cur = getline('.')

    normal! V
    normal ]M
    normal! y
   
    let output = system("cat > " . shellescape(g:vimux_temp_file), @")
    call VimuxTmux('load-buffer ' . g:vimux_temp_file)
    call VimuxTmux('paste-buffer -d -p -t' . g:VimuxRunnerIndex)
    call VimuxSendKeys('Enter')

    normal ]M
    normal! j
endfunction


autocmd FileType python nnoremap <buffer><leader>gp :pop<cr>
autocmd FileType python nnoremap <buffer><leader>gg :call jedi#goto_definitions()<cr>
autocmd FileType python nnoremap <buffer><leader>gl :call jedi#usages()<cr>
autocmd FileType python nnoremap <buffer><leader>gr :call jedi#rename()<cr>
autocmd FileType python nnoremap <buffer><leader>rr :call MySendPyBlock()<cr>

"
" R
"
function! MySendRBlock()
    
    call VimuxSendKeys('Enter')
    let cur = getline('.')

    if stridx(cur, 'function') != -1 || stridx(cur, 'list(') != -1
        normal! V
        normal! ][
        normal! y
       
        let output = system("cat > " . shellescape(g:vimux_temp_file), @")
        call VimuxTmux('load-buffer ' . g:vimux_temp_file)
        call VimuxTmux('paste-buffer -d -p -t' . g:VimuxRunnerIndex)
        call VimuxSendKeys('Enter')

        normal! ][
        normal! j
    else
        normal! V
        normal! }
        normal! y

        let output = system("cat > " . shellescape(g:vimux_temp_file), @")
        call VimuxTmux('load-buffer ' . g:vimux_temp_file)
        call VimuxTmux('paste-buffer -d -p -t' . g:VimuxRunnerIndex)
        call VimuxSendKeys('Enter')

        normal! }
        normal! j
    endif
endfunction

autocmd FileType r,rmd nnoremap <buffer><leader>rr :call MySendRBlock()<cr>
