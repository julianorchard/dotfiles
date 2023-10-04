-- Run terraform format on save
vim.api.nvim_create_autocmd(
  "BufWritePost", {
    command = [[ :silent! !terraform fmt % ]]
  }
)

vim.opt.commentstring = "#%s"
