vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2

vim.opt.commentstring = "#%s"

-- Run terraform format on save (replaced with Conform.nvim):
-- vim.api.nvim_create_autocmd(
--   "BufWritePost", {
--     command = [[ :silent! !terraform fmt % ]]
--   }
-- )
