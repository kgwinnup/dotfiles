
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
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'rust-lang/rust.vim'
call plug#end()

filetype off
syntax on
filetype plugin indent on
" colorscheme gruvbox
colorscheme gruvbox

set nobackup
set nowritebackup
set nocompatible
set modelines=0
set number
set ruler
set encoding=utf-8
set wrap
set textwidth=80
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

" fzf
let g:fzf_layout = { 'down': '~40%' }
let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow'

function! FzfFileOpenSink(line)
    " Split the line by colon
    let parts = split(a:line, ':')

    " Check if the split result has at least two parts (filename and line number)
    if len(parts) >= 2
        " Construct the Vim command to open the file at the specific line
        let fileCmd = parts[0] 
        let lineNum = parts[1] 

        " Execute the command
        execute 'edit ' . '+' . lineNum . ' ' fileCmd
    endif
endfunction

function! s:RgFZF(query)
    let command_fmt = 'rg --ignore-file .gitignore --column --line-number --no-heading --color=never --smart-case %s'
    let initial_command = printf(command_fmt, shellescape(a:query))
    let reload_command = printf(command_fmt, '{q}')
    let options = '--delimiter : --nth 4'
    call fzf#run(fzf#wrap({
        \ 'source': initial_command,
        \ 'sink': function('FzfFileOpenSink'),
        \ 'options': options,
        \ 'reload': reload_command
        \ }))
endfunction

command! -nargs=* Rg call s:RgFZF(<q-args>)

" Use tab for trigger completion with characters ahead and navigate
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

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
nnoremap <leader>nf :FZF<cr>
nnoremap <leader>ng :Rg<cr>
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

nnoremap <leader>t :call CocActionAsync('doHover')<cr>
nnoremap <leader>gg <Plug>(coc-definition)
nnoremap <leader>gp <c-o>
nnoremap <leader>gl <Plug>(coc-references)
nnoremap <leader>gr <Plug>(coc-rename)

"
" R
"
function! MySendRBlock()
    
    call VimuxSendKeys('Enter')
    let cur = getline('.')

    if stridx(cur, 'function') != -1 || stridx(cur, 'list(') != -1
        normal V
        normal ][
        normal y
       
        let output = system("cat > " . shellescape(g:vimux_temp_file), @")
        call VimuxTmux('load-buffer ' . g:vimux_temp_file)
        call VimuxTmux('paste-buffer -d -p -t' . g:VimuxRunnerIndex)
        call VimuxSendKeys('Enter')

        normal ][
        normal j
    else
        normal V
        normal }
        normal y

        let output = system("cat > " . shellescape(g:vimux_temp_file), @")
        call VimuxTmux('load-buffer ' . g:vimux_temp_file)
        call VimuxTmux('paste-buffer -d -p -t' . g:VimuxRunnerIndex)
        call VimuxSendKeys('Enter')

        normal! }
        normal! j
    endif
endfunction

autocmd FileType r setlocal shiftwidth=2 softtabstop=2 expandtab
autocmd FileType r,rmd nnoremap <buffer><leader>rr :call MySendRBlock()<cr>
