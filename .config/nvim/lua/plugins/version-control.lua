local function fugimap(a, b, c)
  local cmd = string.format([[ <cmd>Git %s<cr> ]], c)
  vim.keymap.set(a, b, cmd, {
    noremap = true, silent = true,
  })
end

local function p4map(a, b, c)
  local cmd = string.format([[ <cmd>Vp4%s<cr> ]], c)
  vim.keymap.set(a, b, cmd, {
    noremap = true, silent = true,
  })
end

return {
  {
    "tpope/vim-fugitive",
    init = function()
      fugimap("n", "<leader>gb", "blame")
      fugimap("n", "<leader>gd", "diff")
      fugimap("n", "<leader>gl", "log")
      fugimap("n", "<leader>gs", "status")
    end
  },
  {
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
  },
  "sindrets/diffview.nvim",
  {
    "ngemily/vim-vp4",
    init = function()
      p4map("n", "<leader>4c", "Change")
      p4map("n", "<leader>4d", "Describe")
      p4map("n", "<leader>4e", "Edit")
      p4map("n", "<leader>4r", "Reopen")
      p4map("n", "<leader>4u", "Revert")
    end
  }
}
