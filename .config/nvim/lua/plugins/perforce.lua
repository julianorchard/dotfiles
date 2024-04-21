local function p4map(a, b, c)
  local cmd = string.format([[ <cmd>Vp4%s<cr> ]], c)
  vim.keymap.set(a, b, cmd, {
    noremap = true, silent = true,
  })
end

return {
  "ngemily/vim-vp4",
  init = function ()
    p4map("n", "<leader>4c", "Change")
    p4map("n", "<leader>4d", "Describe")
    p4map("n", "<leader>4e", "Edit")
    p4map("n", "<leader>4r", "Reopen")
    p4map("n", "<leader>4u", "Revert")
  end
}
