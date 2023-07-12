-- General
vim.opt.encoding = "UTF-8"
vim.opt.history = 1000
vim.opt.lazyredraw = false

-- Shared clipboard
vim.opt.clipboard = 'unnamed'

-- Text width/wrap/offsets
vim.opt.scrolloff = 13
vim.opt.textwidth = 80
vim.opt.wrap = false

-- Line numbering
vim.opt.number = true
vim.opt.relativenumber = true

-- Indentation
vim.opt.autoindent = true
vim.opt.smartindent = true

-- Tab
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2

-- Char icons
vim.opt.list = true
vim.opt.listchars = "tab:| ,nbsp:␣"

-- Remove blank line endings on save
vim.api.nvim_create_autocmd(
  "BufWritePre",
  { command = [[ :%s/\s\+$//ge ]]  }
)

