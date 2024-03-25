local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  use 'neovim/nvim-lspconfig' -- Collection of configurations for the built-in LSP client
  use 'vim-airline/vim-airline'
  use 'vim-airline/vim-airline-themes'

  use 'benmills/vimux'
  -- use 'morhetz/gruvbox'
  use "ellisonleao/gruvbox.nvim"
  use 'ervandew/supertab'
  use 'scrooloose/nerdtree'
  -- this is more up to date than the default rust.vim that comes with neovim
  use 'rust-lang/rust.vim'
  -- tabular is required for vim-markdown
  use 'godlygeek/tabular'
  use 'preservim/vim-markdown'
  -- golang
  use 'fatih/vim-go'
  use 'nvim-treesitter/nvim-treesitter'

  use 'junegunn/fzf' 
  use 'tpope/vim-fugitive'
  use 'github/copilot.vim'
end)

require("gruvbox").setup({
    -- contrast = "soft",
    overrides = {
        -- Function = { fg = "#fabd2f" },
        -- Type = { fg = "#d3869b" },
    }
})

vim.mapleader = " "
vim.g.mapleader = " "

vim.cmd("set formatoptions-=t")

vim.opt.showtabline = 0
vim.opt.ttimeoutlen = 2000
vim.opt.timeoutlen = 2000
vim.opt.shortmess = "at"
vim.opt.ruler = true
vim.opt.number = true
vim.opt.autoindent = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.autoread = true
vim.opt.expandtab = true
vim.opt.fileformats = "unix,dos,mac"
vim.opt.showmatch = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.wildmenu = true
vim.opt.wildmode = "list:longest,full"
vim.opt.mouse = "a"
vim.opt.backspace = "indent,eol,start"
vim.opt.clipboard = "unnamedplus"
vim.opt.background = "dark"
vim.opt.textwidth = 72
vim.cmd("colorscheme gruvbox")
vim.cmd("hi SpellBad cterm=underline,bold ctermfg=red")
vim.opt.cursorline = true
vim.opt.guicursor = ""
--
local opts = { noremap=true, silent=true }

vim.api.nvim_set_keymap("n", "<leader>np", ":bprevious<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>nn", ":bnext<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>nd", ":bdelete<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>no", ":only<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ns", "<C-W><C-W>", opts)
vim.api.nvim_set_keymap("n", "<leader>nf", ":FZF<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ng", ":Rg<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>nt", ":NERDTreeToggle<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>j", "<C-d>", opts)
vim.api.nvim_set_keymap("n", "<leader>k", "<C-u>", opts)
vim.api.nvim_set_keymap("n", "<leader>=", "<C-w>=", opts)
vim.api.nvim_set_keymap("n", "<leader>-", ":res -5<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>+", ":res +5<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ss", ":setlocal spell spelllang=en_us<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>sf", ":setlocal nospell<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>sn", "]s", opts)
vim.api.nvim_set_keymap("n", "<leader>sp", "[s", opts)
vim.api.nvim_set_keymap("n", "<leader>sr", "z=", opts)
vim.api.nvim_set_keymap("n", "<leader>sa", "zg", opts)
vim.api.nvim_set_keymap("n", "<leader>vp", ":VimuxPromptCommand<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>vl", ":VimuxRunLastCommand<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ms", ":silent Git<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>mp", ":silent Git push<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>mb", ":silent Git blame<cr>", opts)
vim.api.nvim_set_keymap('n', '<leader>dd', ':silent source ~/.config/nvim/init.lua<cr>', opts)

-- markdown
vim.g.vim_markdown_folding_disabled = 1
-- this concels the ticks in code snippets
-- vim.opt.conceallevel = 2

-- airline
vim.cmd("let g:airline#extensions#tabline#enabled = 0")
vim.cmd("let g:airline#extensions#tabline#show_buffers = 1")
vim.cmd("let g:airline_powerline_fonts = 1")
vim.cmd("let g:airline_theme = 'minimalist'")

-- table mode
vim.cmd("let g:table_mode_disable_mappings = 1")
-- vim.cmd("let g:table_mode_disable_tableize_mappings = 1")

-- supertab for lsp tab completion
vim.g.SuperTabDefaultCompletionType = "<c-x><c-o>"
vim.g.SuperTabCrMapping = 1
vim.g.SuperTabClosePreviewOnPopupClose = 1

-- fzf
vim.g.fzf_layout = { down = '~40%' }
vim.env.FZF_DEFAULT_COMMAND = 'rg --files --follow'

local rgfzf = [[
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
]]

vim.cmd(rgfzf)

local on_attach = function(client, bufnr)

    if client.name == 'ruff_lsp' then
        -- Disable hover in favor of Pyright
        client.server_capabilities.hoverProvider = false
    end

    if client.supports_method("textDocument/formatting") then
        vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
        vim.api.nvim_create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
                vim.lsp.buf.format()
            end,
        })
    end

    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>t', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gg', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gp', ':pop<cr>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gl', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gr', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gh', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
end

local servers = { 'rust_analyzer', 'clangd', 'gopls', 'pyright', 'ruff_lsp', 'ocamllsp' }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    flags = {
      -- This will be the default in neovim 0.7+
      debounce_text_changes = 150,
    },
    settings = {
        -- Configure formatting options
        -- You may need to adjust this based on the specific settings supported by ruff_lsp
        python = {
            formatting = {
                provider = "ruff_lsp"
            }
        }
    }
}
end

require('lspconfig').pyright.setup {
  settings = {
    pyright = {
      -- Using Ruff's import organizer
      disableOrganizeImports = true,
    },
    python = {
      analysis = {
        -- Ignore all files for analysis to exclusively use Ruff for linting
        ignore = { '*' },
      },
    },
  },
}

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "go", "python", "ocaml" },
  sync_install = false,
  auto_install = true,
  ignore_install = { "javascript" },

  highlight = {
    -- `false` will disable the whole extension
    enable = true,
    disable = { "rust", "markdown", "lua" },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}

-- nerd tree settings
vim.g.NERDTreeQuitOnOpen = 1
vim.cmd("let NERDTreeIgnore = ['\\.pyc$', '^__pycache__$', '^node_modules']")

-- rust specific configs
vim.g.rust_recommended_style = 1
vim.g.rustfmt_autosave = 1

-- tmux operations
vim.g.tmux_last_command = ""
vim.g.compile_last_command = ""


function after_save()
    vim.cmd("silent !isort %")
    vim.cmd("edit %")
    vim.api.nvim_feedkeys("<CR>", "n", true)
end

vim.cmd('autocmd BufWritePost *.py lua after_save()')


