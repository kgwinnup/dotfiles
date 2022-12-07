local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  use 'neovim/nvim-lspconfig' -- Collection of configurations for the built-in LSP client
  use 'vim-airline/vim-airline'
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
  use 'ido-nvim/ido.nvim'
  use 'tpope/vim-fugitive'
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
vim.api.nvim_set_keymap("n", "<leader>nf", ":Ido std.git_files<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ng", ":Ido std.git_grep<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>nb", ":Ido std.buffer<cr>", opts)
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
vim.api.nvim_set_keymap("n", "<leader>rr", ":silent lua tmux_send_buf()<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>vp", ":silent lua tmux_send_command('')<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>vl", ":silent lua tmux_send_last_command()<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>vu", ":silent lua send_command()<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>ml", ":silent lua git_log2()<cr>", opts)
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
vim.cmd("let g:airline#extensions#tabline#show_buffers=1")
vim.cmd("let g:airline_powerline_fonts=1")

-- table mode
vim.cmd("let g:table_mode_disable_mappings = 1")
-- vim.cmd("let g:table_mode_disable_tableize_mappings = 1")

-- supertab for lsp tab completion
vim.g.SuperTabDefaultCompletionType = "<c-x><c-o>"
vim.g.SuperTabCrMapping = 1
vim.g.SuperTabClosePreviewOnPopupClose = 1

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

local servers = { 'rust_analyzer', 'clangd', 'gopls' }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    flags = {
      -- This will be the default in neovim 0.7+
      debounce_text_changes = 150,
    }
  }
end

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "go" },
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

local ido = require("ido")
ido.setup {
    prompt = "Ido: ",
    render = require('ido.render').vertical,
    ignorecase = true,
    mappings = {
        ["<c-k>"] = ido.delete.line.backward,
        ["<c-g>"] = ido.stop,
        ["<up>"] = ido.prev,
        ["<down>"] = ido.next
    }
}

-- nerd tree settings
vim.g.NERDTreeQuitOnOpen = 1

-- rust specific configs
vim.g.rust_recommended_style = 1
vim.g.rustfmt_autosave = 1

-- tmux operations
vim.g.tmux_last_command = ""
vim.g.compile_last_command = ""

function git_log2()
    local commit = ido.start(vim.fn.systemlist("git log --format='%h%d %an %s %cr'"), {prompt = "Git Log: "})
    if commit then
        commit = commit:gsub(" .*", "")
        vim.cmd("edit "..vim.fn.FugitiveFind(commit))
    end
end

function tmux_window_exists()
    output = vim.fn.system("tmux list-panes |grep 1:")
    print(output)
    if output == "" then
        return false
    else
        return true
    end
end

function tmux_toggle()
    if not tmux_window_exists() then
        if vim.fn.winwidth(0) > 180 or vim.fn.winheight(0) * 2 > vim.fn.winwidth(0) then
            vim.g.tmux_session = vim.fn.system("tmux split-window -h")
        else
            vim.g.tmux_session = vim.fn.system("tmux split-window")
        end
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
            vim.fn.system("tmux send-keys -t 1 " .. "'\n'")
            vim.fn.system("tmux send-keys -t 1 " .. "'" .. txt .. "\n'")
        else
            tmux_toggle()
            vim.fn.system("tmux send-keys -t 1 " .. "'\n'")
            vim.fn.system("tmux send-keys -t 1 " .. "'" .. txt .. "\n'")
        end
        vim.fn.execute("normal <cr>")
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
        if v ~= "" then
            tmux_send_command(v)
        end
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

function send_command()
    last = vim.g.compile_last_command
    input = vim.fn.input("cmd: ", last, "file")
    vim.fn.execute("echon ''\n")

    if input ~= "" then
        kill_all_terms()
        vim.g.compile_last_command = input
        if vim.fn.winwidth(0) > 180 or vim.fn.winheight(0) * 2 > vim.fn.winwidth(0) then
            vim.fn.execute("vsplit")
        else
            vim.fn.execute("split")
        end
        vim.fn.execute("terminal " .. input)
        vim.fn.execute("normal G")
        vim.fn.execute("wincmd w")
    end
end

function kill_all_terms() 
    list = vim.fn.execute("ls")
    for s in list:gmatch("[^\r\n]+") do
        if s:find('term://') ~= nil then
            id = string.sub(s, s:find('[0-9]+'))
            vim.fn.execute(id .. "bw")
        end
    end
end

