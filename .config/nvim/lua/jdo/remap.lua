vim.g.mapleader = " "

-- Graphical Up and Down
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

-- TODO: Set <leader>x to execute script if shell, python, etc.

-- netrw Open
vim.keymap.set("n", "<leader>d", vim.cmd.Ex)

-- Double Space to CMD
vim.keymap.set("n", "<leader> ", ":")

-- Tabbing
vim.keymap.set("v", "<tab>", ">vgv")
vim.keymap.set("v", "<s-tab", "<vgv")

-- ... partly because fun, but also to learn
vim.keymap.set("v", "<leader>r13", function ()
  vim.cmd("'<,'>s/t/p/g")
end)

vim.keymap.set("n", "<leader>sv", function ()
  vim.api.nvim_command("botright vsplit new")
end)

vim.keymap.set("n", "<leader>h", "<C-w>h")
vim.keymap.set("n", "<leader>j", "<C-w>j")
vim.keymap.set("n", "<leader>k", "<C-w>k")
vim.keymap.set("n", "<leader>l", "<C-w>l")

