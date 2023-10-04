return {
  "X3eRo0/dired.nvim",
  dependencies = "MunifTanjim/nui.nvim",
  opts = {
    path_separator = "/",
    show_banner = false,
    show_hidden = true,
    show_dot_dirs = true,
    show_colors = true,
  },
  keys = {
    { "<leader>,", "<cmd>Dired<cr>", desc = "Open Dired" },
  },
  config = function()
    require("dired").setup()
  end
}
