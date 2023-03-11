-- Load custom treesitter grammar for org filetype
require("orgmode").setup_ts_grammar()

-- Treesitter configuration
require("nvim-treesitter.configs").setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop,
  -- highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    -- Required for spellcheck, some LaTex highlights and
    -- code block highlights that do not have ts grammar
    additional_vim_regex_highlighting = {"org"},
  },
  ensure_installed = {"org"}, -- Or run :TSUpdate org
}

require("orgmode").setup({
  org_agenda_files = {"~/org/*"},
  org_default_notes_file = "~/org/personal.org",
  org_hide_leading_stars = true,

  -- Work in progress:
  org_custom_exports = {
    f = {
      label = 'Export to PDF.',
      action = function(exporter)
        local current_file = vim.api.nvim_buf_get_name(0)
        local target = vim.fn.fnamemodify(current_file, ':p:r')..'.pdf'
        local command = {'pandoc', current_file, '-o', target}
        local on_success = function(output)
          print('Success!')
          vim.api.nvim_echo({{ table.concat(output, '\n') }}, true, {})
        end
        local on_error = function(err)
          print('Error!')
          vim.api.nvim_echo({{ table.concat(err, '\n'), 'ErrorMsg' }}, true, {})
        end
        return exporter(command , target, on_success, on_error)
      end
    }
  }
})
