function BgNunify(selector)
  -- Set any element input as a no background
  vim.api.nvim_set_hl(0, selector, {
    bg      = "none",
    ctermbg = "none"
  })
end

function ColourMyPencils(pencils)
  pencils = pencils or "catppuccin"
  vim.cmd.colorscheme(pencils)

  local elms = {"Normal", "NormalFloat", "NormalNC", "EndOfBuffer"}
  for _, elm in ipairs(elms) do
    BgNunify(elm)
  end
end

return {

-- Pinkmare:
--   "matsuuu/pinkmare",
--   priority = 1000,
--   config = function()
--     vim.opt.tgc = true
--     ColourMyPencils("pinkmare")
--   end

-- Catppuccin:
-- "catppuccin/nvim",
-- priority = 1000,
-- config = function()
--   require("catppuccin").setup({
--     flavour = "macchiato",
--     background = {
--       light = "latte",
--       dark = "macchiato"
--     },
--     no_italic = false,
--     transparent_background = true,
--     cmp = true,
--     gitsigns = true,
--   })
--   vim.cmd.colorscheme "catppuccin"
--   -- NOTE: If I change colourscheme this might be required:
--   -- vim.api.nvim_set_hl(0, "Normal", {
--   --     bg = "none",
--   --     ctermbg = "none"
--   -- })
--   -- vim.api.nvim_set_hl(0, "NormalFloat", {
--   --     bg = "none",
--   --     ctermbg = "none"
--   -- })
--   -- vim.api.nvim_set_hl(0, "NormalNC", {
--   --     bg = "none",
--   --     ctermbg = "none"
--   -- })
-- end,
}
