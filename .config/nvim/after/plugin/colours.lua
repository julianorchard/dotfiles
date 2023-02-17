require("catppuccin")

function Colours(colour)
  colour = colour or "catppuccin"
  vim.cmd.colorscheme(colour)
  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

Colours()
