
local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  use 'neovim/nvim-lspconfig' -- Collection of configurations for the built-in LSP client
  use 'morhetz/gruvbox'
  use 'ervandew/supertab'
end)

-- vim.opt.backupdir = ~/.config/nvim/backups
-- vim.opt.dir = ~/.config/nvim/backups/
vim.mapleader = " "

vim.g.mapleader = " "
vim.g.netrw_winsize = 20
vim.g.SuperTabDefaultCompletionType = "<c-x><c-o>"

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
vim.cmd("colorscheme gruvbox")
vim.opt.colorcolumn = "0"

vim.api.nvim_set_keymap("n", "<leader>np", ":bprevious<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>nn", ":bnext<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>nd", ":bdelete<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>no", ":only<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>ns", "<C-W><C-W>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>j", "<C-d>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>k", "<C-u>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>=", "<C-w>=", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>-", ":res -5<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>+", ":res +5<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>nt", ":Lexplore<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>ss", ":setlocal spell spelllang=en_us<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>sf", ":setlocal nospell", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>sn", "]s", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>sp", "[s", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>sr", "z=", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>sa", "zg", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>vp", ":lua tmux_send_command('')<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>vl", ":lua tmux_send_last_command()<cr>", { noremap = true})
vim.api.nvim_set_keymap("n", "<leader>mb", ":lua tmux_send_command('git blame -L ' .. vim.fn.line('.') .. ',' .. vim.fn.line('.') .. ' ' .. vim.fn.expand('%:p'))<cr>", { noremap = true})

-- rust specific configs
vim.g.rust_recommended_style = 1
vim.g.rustfmt_autosave = 1
vim.g.rustfmt_command = 'rustfmt --force'

vim.g.tmux_last_command = ""

function tmux_window_exists()
    output = vim.fn.system("tmux list-panes |grep 1:")
    if output == "" then
        return false
    else
        return true
    end
end

function tmux_toggle() 
    if not tmux_window_exists() then
        vim.g.tmux_session = vim.fn.system("tmux split-window -h")
        vim.fn.system("tmux select-pane -L")
    end
end

function tmux_send_command(txt)
    vim.g.tmux_last_command = txt

    if txt == "" then
        input = vim.fn.input("tmux input: ")
        if input == "" then
        else
            tmux_send_command(input)
        end 
    else
        if tmux_window_exists() then
            vim.fn.system("tmux send-keys -t 1 " .. "'" .. txt .. "\n'") 
        else
            toggle_tmux()
            vim.fn.system("tmux send-keys -t 1 " .. "'" .. txt .. "\n'") 
        end
    end
end

function tmux_send_last_command() 
    tmux_send_command(vim.g.tmux_last_command) 
end

local opts = { noremap=true, silent=true }

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>t', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gg', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gp', ':pop<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gl', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gh', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { 'rust_analyzer' }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    flags = {
      -- This will be the default in neovim 0.7+
      debounce_text_changes = 150,
    }
  }
end

