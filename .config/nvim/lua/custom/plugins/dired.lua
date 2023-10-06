return {
  "X3eRo0/dired.nvim",
  dependencies = "MunifTanjim/nui.nvim",
  init = function()
    require("dired").setup({
      path_separator = "/",
      show_banner = false,
      show_hidden = true,
      show_dot_dirs = true,
      show_colors = true,
    })
    vim.keymap.set("n", "<leader>.", "<cmd>Dired<cr>")
  end,
}
