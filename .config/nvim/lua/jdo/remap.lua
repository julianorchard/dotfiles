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

