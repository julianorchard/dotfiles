vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2

vim.opt.commentstring = "#%s"

-- Run terragrunt format on save
-- vim.api.nvim_create_autocmd(
--   "BufWritePost", {
--     command = [[ :silent! !terragrunt hclfmt --terragrunt-hclfmt-file % ]]
--   }
-- )
