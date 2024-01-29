local actions = require("telescope.actions")
require("telescope").setup {
  defaults = {
    mappings = {
      i = {
        ["<C-u>"] = false,
        ["<C-d>"] = false,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
      },
    },
  },
  pickers = {
    find_files = {
      hidden = true,
      theme = "ivy"
    },
    git_files = {
      theme = "ivy"
    }
  }
}

-- Edited from the other (probably nicer...) way of doing it:
-- https://github.com/nvim-telescope/telescope.nvim/wiki/Configuration-Recipes#falling-back-to-find_files-if-git_files-cant-find-a-git-directory
vim.keymap.set("n", "<Leader>ff", function ()
  local is_inside_work_tree = {}
  local opts = {}
  local cwd = vim.fn.getcwd()
  if is_inside_work_tree[cwd] == nil then
    vim.fn.system("git rev-parse --is-inside-work-tree")
    is_inside_work_tree[cwd] = vim.v.shell_error == 0
  end
  if is_inside_work_tree[cwd] then
    require("telescope.builtin").git_files(opts)
  else
    require("telescope.builtin").find_files(opts)
  end
end, {
  noremap = true,
  silent = true
})

-- Enable telescope fzf native, if installed
pcall(require("telescope").load_extension, "fzf")

-- Find old files (<leader>fh == find history)
vim.keymap.set("n", "<leader>fh", require("telescope.builtin").oldfiles)

-- Find in buffers (<leader>b == buffers)
vim.keymap.set("n", "<leader><space>", require("telescope.builtin").buffers)

-- fzf in the current buffer
vim.keymap.set("n", "<leader>/", function()
  require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown {
    winblend = 10,
    previewer = false,
  })
end)

local tb = require("telescope.builtin")
vim.keymap.set("n", "<leader>gf", tb.git_files)
vim.keymap.set("n", "<leader>fa", tb.find_files)
vim.keymap.set("n", "<leader>fw", tb.grep_string)
vim.keymap.set("n", "<leader>fg", tb.live_grep)
vim.keymap.set("n", "<leader>fd", tb.diagnostics)
vim.keymap.set("n", "<leader>fr", tb.resume)
