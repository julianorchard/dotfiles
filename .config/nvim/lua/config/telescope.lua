local M = {}

M.setup = function()
  local actions = require("telescope.actions")

  require("telescope").setup {
    defaults = {
      mappings = {
        i = {
          ["<C-u>"] = false,
          ["<C-d>"] = false,
          ["<C-j>"] = actions.move_selection_next,
          ["<C-k>"] = actions.move_selection_previous,
          ["<C-e>"] = actions.file_edit,
        },
      },
    },
    pickers = {
      find_files = {
        hidden = true,
      },
    }
  }

  local t = require("telescope.builtin")
  local opts = require("telescope.themes").get_ivy({})

  local function wrapiscope(cmd)
    local pog = opts
    cmd(pog)
  end

  -- Edited from the other (probably nicer...) way of doing it:
  -- https://github.com/nvim-telescope/telescope.nvim/wiki/Configuration-Recipes#falling-back-to-find_files-if-git_files-cant-find-a-git-directory
  vim.keymap.set("n", "<Leader>ff", function()
    local is_inside_work_tree = {}
    local cwd = vim.fn.getcwd()
    if is_inside_work_tree[cwd] == nil then
      vim.fn.system("git rev-parse --is-inside-work-tree")
      is_inside_work_tree[cwd] = vim.v.shell_error == 0
    end
    if is_inside_work_tree[cwd] then
      t.git_files(opts)
    else
      t.find_files(opts)
    end
  end, {
    noremap = true,
    silent = true
  })

  -- Enable telescope fzf native, if installed
  pcall(require("telescope").load_extension, "fzf")

  -- Find old files (<leader>fh == find history)
  vim.keymap.set("n", "<leader>fh", function() wrapiscope(t.oldfiles) end)

  -- Find in buffers (<leader>b == buffers)
  vim.keymap.set("n", "<leader><space>", function() wrapiscope(t.buffers) end)

  -- fzf in the current buffer
  vim.keymap.set("n", "<leader>/", function()
    require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_ivy {
      winblend = 10,
      previewer = false,
    })
  end)

  vim.keymap.set("n", "<leader>gf", function() wrapiscope(t.git_files) end)
  vim.keymap.set("n", "<leader>fo", function() wrapiscope(t.find_files) end)
  vim.keymap.set("n", "<leader>fw", function() wrapiscope(t.grep_string) end)
  vim.keymap.set("n", "<leader>fg", function() wrapiscope(t.live_grep) end)
  vim.keymap.set("n", "<leader>fd", function() wrapiscope(t.diagnostics) end)
  vim.keymap.set("n", "<leader>fr", function() wrapiscope(t.resume) end)
end

M.setup()

return M
