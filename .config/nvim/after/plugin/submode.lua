-- Rewritten from https://ddrscott.github.io/blog/2016/making-a-window-submode/
-- but with neovim and lua in mind

vim.g.submode_always_show_submode = 1
vim.g.submode_timeout = true
vim.g.submode_timeoutlen = 1000

vim.fn["submode#enter_with"](
  "window", "n", "", "<C-w>"
)
vim.fn["submode#leave_with"](
  "window", "n", "", "<Esc>"
)

local chars = {
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
  "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
}
-- Use of _'s thanks to @bertiepwhite
for _, v in ipairs(chars) do
  vim.fn["submode#map"]("window", "n", "", v, "<C-w>" .. v)
  vim.fn["submode#map"]("window", "n", "", v:upper(), "<C-w>" .. v:upper())
end

local symbols = { "=", "_", "+", "-", "<", ">" }
for _, v in ipairs(symbols) do
  vim.fn["submode#map"]("window", "n", "", v, "<C-w" .. v)
end



