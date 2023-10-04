-- JavaScript has no official style guide
-- but definitely no longer than PHP lines (120)
vim.opt.colorcolumn = { "80", "120" }

-- Run prettier on save
vim.api.nvim_create_autocmd(
    "BufWritePost", {
        command = [[ :silent! !prettier % ]]
    }
)
