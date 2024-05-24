local telescope = require("telescope")

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
  -- Wrap telescope cmd with telescope opts (above)
  local function t_cmd_with_opts(cmd) cmd(opts) end

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

  -- Find in buffers (<leader><leader> == buffers)
  vim.keymap.set("n", "<leader><space>", function()
    local bufs = vim.fn.getbufinfo({ buflisted = 1 })
    if #bufs > 2 then
      t_cmd_with_opts(t.buffers)
    else
      vim.cmd.bnext()
    end
  end)

  -- fzf in the current buffer
  vim.keymap.set("n", "<leader>/", function()
    require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_ivy {
      -- winblend = 10,
      previewer = false,
    })
  end)

  -- Other than that, I only really live_grep stuff
  vim.keymap.set("n", "<leader>fg", function() t_cmd_with_opts(t.live_grep) end)
  vim.keymap.set("n", "<leader>fd", function() t_cmd_with_opts(t.diagnostics) end)
  vim.keymap.set("n", "<leader>fh", function() t_cmd_with_opts(t.oldfiles) end)

  -- Just in case I want to run these manually
  vim.keymap.set("n", "<leader>fgit", function() t_cmd_with_opts(t.git_files) end)
  vim.keymap.set("n", "<leader>files", function() t_cmd_with_opts(t.find_files) end)
end

M.setup()

return M
