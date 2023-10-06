-- Python should be limited to 79 for everything,
-- 72 for only docstrings and comments
vim.opt.colorcolumn = { "72", "79" }

-- Run black formatter on save
vim.api.nvim_create_autocmd(
  "BufWritePost", {
    command = [[ :silent! !black % ]]
  }
)
