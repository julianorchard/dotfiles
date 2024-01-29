return {
  "romgrk/barbar.nvim",
  dependencies = {
    "lewis6991/gitsigns.nvim",
    "nvim-tree/nvim-web-devicons",
  },
  init = function()
    vim.g.barbar_auto_setup = false
    vim.keymap.set("n", "<leader>q", "<cmd>BufferClose<cr>")
    vim.keymap.set("n", "<leader>h", "<cmd>BufferNext<cr>")
    vim.keymap.set("n", "<leader>l", "<cmd>BufferPrevious<cr>")
  end,
  opts = {
    animation = false,
    tabpages = true,
    focus_on_close = "previous",
    icons = {
      buffer_index = false,
      buffer_number = false,
      button = "",
      separator = {
        left = "",
        right = ""
      },
    },
    insert_at_start = true,
    maximum_padding = 1,
    maximum_length = 15,
    no_name_title = "Untitled",
  },
  version = "^1.7.0",
}
