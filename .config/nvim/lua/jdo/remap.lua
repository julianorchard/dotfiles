vim.g.mapleader = " "

-- Graphical Up and Down
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

-- netrw Open
vim.keymap.set("n", "<leader>d", vim.cmd.Ex)

-- Double Space to CMD
vim.keymap.set("n", "<leader> ", ":")

-- Tabbing
vim.keymap.set("v", "<tab>", ">vgv")
vim.keymap.set("v", "<s-tab", "<vgv")

-- Split Command
vim.keymap.set("n", "<leader>sv", function ()
  vim.api.nvim_command("botright vsplit new")
end)
-- vim.keymap.set("n", "<leader>sh", function()
--   vim.api.nvim_command("bot split new")
-- end)


vim.keymap.set("n", "<leader>h", "<C-w>h")
vim.keymap.set("n", "<leader>j", "<C-w>j")
vim.keymap.set("n", "<leader>k", "<C-w>k")
vim.keymap.set("n", "<leader>l", "<C-w>l")

-- Fugitive
local fug_commit = function (c)
  local is_git_repo = os.execute("git rev-parse --is-inside-work-tree")
  if (is_git_repo == true) then
    return vim.cmd(c)
  else
    return print("We're not inside a git repo.")
  end
end

-- Commit Current File
vim.keymap.set("n", "<leader>gc", function ()
  fug_commit("Git add " .. vim.fn.expand("%"))
end)

