return {
  dir = "~/Code/centre-comment.vim/",
  dev = true,
  config = function ()
    vim.keymap.set("n", "<leader>c-", ":CenterComment -<cr>")
    vim.keymap.set("n", "<leader>c~", ":CenterComment -<cr>")
    vim.keymap.set("n", "<leader>c=", ":CenterComment -<cr>")
    vim.keymap.set("n", "<leader>c/", ":CenterComment -<cr>")
  end
}
