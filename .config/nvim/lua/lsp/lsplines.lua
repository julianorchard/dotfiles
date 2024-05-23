return {
  "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
  config = function()
    require("lsp_lines").setup()
    vim.keymap.set(
      "n",
      "<leader>ll",
      require("lsp_lines").toggle
    )
  end
}
