local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

vim.keymap.set("n", "<leader>ha", mark.add_file)
vim.keymap.set("n", "<leader>hm", ui.toggle_quick_menu)

-- Map every <leader>number key to nav_file(matching number)
for i = 1, 5, 1 do
  vim.keymap.set("n", "<leader>" .. tostring(i), function()
    ui.nav_file(i)
  end)
end

-- Harpoon mode... using SubMode.vim
vim.fn["submode#enter_with"](
  "harpoon", "n", "", "<leader><leader>h"
)
vim.fn["submode#leave_with"](
  "harpoon", "n", "", "<Esc>"
)

vim.fn["submode#map"]("window", "n", "", "5", "<leader>5")


for i = 1, 4, 1 do
  vim.fn["submode#map"]("window", "n", "", tostring(i), "<leader>" .. tostring(i))
end
