-- https://go.dev/doc/effective_go#formatting
-- "Go has no line length limit. Don't worry about overflowing a punched card.
--  If a line feels too long, wrap it and indent with an extra tab. "
vim.opt.colorcolumn = ""

-- Indentation
vim.bo.expandtab = false
vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4
vim.bo.tabstop = 4

-- vim.api.nvim_create_autocmd(
--   "BufWritePost", {
--     command = function ()
--        if string.match(vim.fn.expand("%:p"), "cricwatch") then
--             return "test"
--          -- return [[ :!go test ]]
--        end
--     end
--   }
-- )


vim.keymap.set("n", "<leader>ee", "oif err != nil {<cr><cr>}<esc>ki<tab>return err<esc>jj")
