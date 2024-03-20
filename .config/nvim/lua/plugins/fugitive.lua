local function fugimap(a, b, c)
  local cmd = string.format([[ <cmd>Git %s<cr> ]], c)
  vim.keymap.set(a, b, cmd, {
    noremap = true, silent = true,
  })
end

return {
  "tpope/vim-fugitive",
  init = function ()
    fugimap("n", "<leader>gb", "blame")
    fugimap("n", "<leader>gd", "diff")
    fugimap("n", "<leader>gl", "log")
    fugimap("n", "<leader>gs", "status")
  end
}
