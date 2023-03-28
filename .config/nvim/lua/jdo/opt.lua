-- General
vim.opt.encoding = "UTF-8"
vim.opt.history = 1000
vim.opt.wrap = true
vim.opt.textwidth = 80
vim.opt.scrolloff = 13
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.clipboard = "unnamedplus"
vim.opt.lazyredraw = false

-- Tab
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

-- Remove blank line endings on save
vim.api.nvim_create_autocmd(
  "BufWritePre",
  { command = [[ :%s/\s\+$//ge ]]  }
)

