
local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  use 'neovim/nvim-lspconfig' -- Collection of configurations for the built-in LSP client
  use 'morhetz/gruvbox'
  use 'ervandew/supertab'
  use 'scrooloose/nerdtree'
  use 'rust-lang/rust.vim'
end)

-- vim.fn.system("mkdir -p ~/.config/nvim/backups")
-- vim.opt.backupdir = vim.env.HOME .. "/.config/nvim/backups"
-- vim.opt.dir = vim.env.HOME .. "/.config/nvim/backups/"

vim.mapleader = " "
vim.g.mapleader = " "

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

local opts = { noremap=true, silent=true }

vim.api.nvim_set_keymap("n", "<leader>np", ":bprevious<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>nn", ":bnext<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>nd", ":bdelete<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>no", ":only<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ns", "<C-W><C-W>", opts)
vim.api.nvim_set_keymap("n", "<leader>j", "<C-d>", opts)
vim.api.nvim_set_keymap("n", "<leader>k", "<C-u>", opts)
vim.api.nvim_set_keymap("n", "<leader>=", "<C-w>=", opts)
vim.api.nvim_set_keymap("n", "<leader>-", ":res -5<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>+", ":res +5<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>nt", ":NERDTreeToggle<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ss", ":setlocal spell spelllang=en_us<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>sf", ":setlocal nospell", opts)
vim.api.nvim_set_keymap("n", "<leader>sn", "]s", opts)
vim.api.nvim_set_keymap("n", "<leader>sp", "[s", opts)
vim.api.nvim_set_keymap("n", "<leader>sr", "z=", opts)
vim.api.nvim_set_keymap("n", "<leader>sa", "zg", opts)
vim.api.nvim_set_keymap("n", "<leader>rr", ":lua tmux_send_buf()<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>vp", ":lua tmux_send_command('')<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>vl", ":lua tmux_send_last_command()<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>mb", ":lua tmux_send_command('git blame -L ' .. vim.fn.line('.') .. ',' .. vim.fn.line('.') .. ' ' .. vim.fn.expand('%:p'))<cr>", opts)
vim.api.nvim_set_keymap('n', '<leader>dd', '<cmd>lua vim.diagnostic.open_float()<cr>', opts)


-- supertab for lsp tab completion
vim.g.SuperTabDefaultCompletionType = "<c-x><c-o>"

local on_attach = function(client, bufnr)
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

local servers = { 'rust_analyzer', 'clangd' }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    flags = {
      -- This will be the default in neovim 0.7+
      debounce_text_changes = 150,
    }
  }
end

-- nerd tree settings
vim.g.NERDTreeQuitOnOpen = 1

-- rust specific configs
vim.g.rust_recommended_style = 1
vim.g.rustfmt_autosave = 1

-- tmux operations
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
        if input ~= "" then
            tmux_send_command(input)
        end 
    else
        if tmux_window_exists() then
            vim.fn.system("tmux send-keys -t 1 " .. "'" .. txt .. "\n'") 
        else
            tmux_toggle()
            vim.fn.system("tmux send-keys -t 1 " .. "'" .. txt .. "\n'") 
        end
    end
end

function tmux_send_last_command() 
    tmux_send_command(vim.g.tmux_last_command) 
end

local function tmux_send_block() 
    start = vim.fn.line(".")
    _end = vim.fn.search("^$")
    lines = vim.fn.getbufline(vim.fn.bufnr("%"), start, _end-1)
    for k,v in pairs(lines) do
        tmux_send_command(v)
    end
    vim.fn.execute("normal! :" .. _end)
end

local function tmux_send_function() 
    start = vim.fn.line(".")
    vim.fn.execute("normal! ][")
    _end = vim.fn.line(".")
    lines = vim.fn.getbufline(vim.fn.bufnr("%"), start, _end)
    for k,v in pairs(lines) do
        if v == "" then
            tmux_send_command("\n")
        else 
            tmux_send_command(v)
        end
    end
    vim.fn.execute("normal! :" .. _end)
end

function tmux_send_buf()
    if string.find(vim.fn.getline("."), "function") ~= nil then
        tmux_send_function()
    else
        tmux_send_block()
    end
end





