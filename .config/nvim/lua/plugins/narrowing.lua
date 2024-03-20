return {
  "chrisbra/NrrwRgn",
  init = function()
    vim.keymap.set("v", "nn", "<cmd>'<,'>NR<cr>")
    vim.keymap.set("v", "nf", "<cmd>'<,'>NR!<cr>")
  end,
}
