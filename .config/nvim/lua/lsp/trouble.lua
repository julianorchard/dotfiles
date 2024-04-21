local M = {
  "folke/trouble.nvim",
  opts = {
    icons = false,
    icons_enabled = false,
    fold_open = "v",
    fold_closed = ">",
    signs = require("helpers.icons").trouble
  }
}

M.config = function()
  local t = require("trouble")
  vim.keymap.set("n", "<leader>xx", function() t.toggle() end)
  vim.keymap.set("n", "<leader>xw", function() t.toggle("workspace_diagnostics") end)
  vim.keymap.set("n", "<leader>xd", function() t.toggle("document_diagnostics") end)
  vim.keymap.set("n", "<leader>xq", function() t.toggle("quickfix") end)
  vim.keymap.set("n", "<leader>xl", function() t.toggle("loclist") end)
  vim.keymap.set("n", "gR", function() require("trouble").toggle("lsp_references") end)
end

return M
