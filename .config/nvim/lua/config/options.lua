-- General
vim.opt.encoding = "UTF-8"
vim.opt.history = 1000
vim.opt.lazyredraw = false

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = "a"

-- Text width/wrap/offsets
vim.opt.scrolloff = 13
-- TODO: Line break at a certin point
-- vim.opt.textwidth = 80
vim.opt.wrap = false

-- Line numbering
vim.opt.number = true
vim.opt.relativenumber = true

-- Indentation
vim.opt.autoindent = true
vim.opt.smartindent = true
-- vim.o.breakindent = true

-- Tab
local tab_default = 4
vim.opt.expandtab = true
vim.opt.shiftwidth = tab_default
vim.opt.softtabstop = tab_default
vim.opt.tabstop = tab_default

-- Char icons
vim.opt.list = true
vim.opt.listchars = "tab:| ,nbsp:␣"

-- Spelling
vim.opt.spelllang = {
  "en",
  -- "jp",
}

-- Update time
vim.opt.updatetime = 500

-- Colour column
vim.opt.colorcolumn = "80"

-- Highlight cursor
vim.opt.cursorline = true
vim.opt.cursorcolumn = true

-- Remove blank line endings on save
vim.api.nvim_create_autocmd(
  "BufWritePre",
  { command = [[ :%s/\s\+$//ge ]] }
)

-- Return-cursor
vim.api.nvim_create_autocmd(
  "BufReadPost",
  { command = [[ if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif ]] }
)

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = "yes"

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = "menuone,noselect"

-- Terminal
vim.api.nvim_create_autocmd(
  "TermOpen", {
    command = [[
      :startinsert
      :setlocal nonumber norelativenumber
    ]]
  }
)

-- Tryin to make a change :-\ ………………slime man
vim.opt.statusline = "%#WinSeparator#%{%v:lua.string.rep('—', v:lua.vim.fn.winwidth(0))%}"
vim.opt.laststatus = 0

vim.filetype.add({
  extension = {
    jenkins = "groovy"
  }
})
