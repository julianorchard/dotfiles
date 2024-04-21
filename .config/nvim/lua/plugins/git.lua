local function fugimap(a, b, c)
  local cmd = string.format([[ <cmd>Git %s<cr> ]], c)
  vim.keymap.set(a, b, cmd, {
    noremap = true, silent = true,
  })
end

local M = {}

table.insert(M, {
  "tpope/vim-fugitive",
  init = function()
    fugimap("n", "<leader>gb", "blame")
    fugimap("n", "<leader>gd", "diff")
    fugimap("n", "<leader>gl", "log")
    fugimap("n", "<leader>gs", "status")
  end
})

table.insert(M, {
  "lewis6991/gitsigns.nvim",
  opts = {
    signs = {
      add = { text = "+" },
      change = { text = "~" },
      delete = { text = "_" },
      topdelete = { text = "‾" },
      changedelete = { text = "~" },
    },
  }
})

return M
