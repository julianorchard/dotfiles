local M = {}
local T = {}

function BgNunify(selector)
  vim.api.nvim_set_hl(0, selector, {
    bg      = "none",
    ctermbg = "none"
  })
end

function ColourMyPencils(pencils)
  vim.cmd.colorscheme(pencils)

  local elms = {
    "Normal",
    "NormalFloat",
    "NormalNC",
    "EndOfBuffer"
  }
  for _, elm in ipairs(elms) do
    BgNunify(elm)
  end
end

T.pinkmare = {
  "matsuuu/pinkmare",
  config = function()
    vim.opt.tgc = true
    ColourMyPencils("pinkmare")
  end
}

T.catppuccin = {
  "catppuccin/nvim",
  config = function()
    require("catppuccin").setup({
      flavour = "macchiato",
      background = {
        light = "latte",
        dark = "macchiato"
      },
      no_italic = false,
      transparent_background = true,
      cmp = true,
      gitsigns = true,
    })
    vim.cmd.colorscheme "catppuccin"
  end,
}

for _, v in pairs(T) do
  v.priority = 1000
end

if VimOps.theme == "pinkmare" then
  table.insert(M, T.pinkmare)
elseif VimOps.theme == "catppuccin" then
  table.insert(M, T.catppuccin)
else
  -- Not really tested this stuff yet
  vim.cmd("source /home/julian/.config/nvim/theme.vim")
  return {}
end

return M
