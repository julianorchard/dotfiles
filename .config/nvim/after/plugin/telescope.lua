local telescope = require("telescope")
local built_in = require("telescope.builtin")

local ignore_list = {
  "^.git/",
  "%.otf",
  "%.pdf",
  "%.ttf",
  "%.zip",
}

telescope.setup {
  defaults = {
    selection_strategy = "reset",
  },
  pickers = {
    find_files = {
      hidden = true,
      file_ignore_patterns = ignore_list
    }
    -- TODO grep_string version
  }
}

-- Set space+f+f to find file; if we're in a git repo, use
-- the git_files builtin rather than the standard one.
vim.keymap.set("n", "<leader>ff", function()
    local is_git_repo = os.execute("git rev-parse --is-inside-work-tree")
    if (is_git_repo == true) then
      built_in.git_files({})
    else
      built_in.find_files({})
    end
  end
)
-- If we want to force find all files...
vim.keymap.set("n", "<leader>fa", built_in.find_files, {})

-- This does a grep search for stuff, thanks Primegeans
vim.keymap.set("n", "<leader>/",
  function()
    built_in.grep_string({ search = vim.fn.input("grep > ") })
  end
)

