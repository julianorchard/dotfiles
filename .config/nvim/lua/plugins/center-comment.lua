return {
  "julianorchard/center-comment.vim",
  config = function ()
    vim.keymap.set("n", "<leader>c-", "<cmd>CenterComment -<cr>")
    vim.keymap.set("n", "<leader>c!", "<cmd>CenterComment !<cr>")
    vim.keymap.set("n", "<leader>c=", "<cmd>CenterComment =<cr>")
  end
}
