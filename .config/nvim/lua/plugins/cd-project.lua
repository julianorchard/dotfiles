return {
    "LintaoAmons/cd-project.nvim",
    config = function()
      require("cd-project").setup({
        projects_config_filepath = vim.fs.normalize(vim.fn.stdpath("config") .. "/cd-project.nvim.json"),
        project_dir_pattern = {
          ".git",
          ".gitignore",
          "package.json",
          "README.md",
          "go.mod"
        },
        hooks = {
          {
            callback = function(dir)
              vim.notify("switched to dir: " .. dir)
            end,
          },
        },
        projects_picker = "telescope",
      })
      vim.keymap.set("n", "<leader>cd", ":CdProject<cr>")
    end,
  }
