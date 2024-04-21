return {
  "stevearc/conform.nvim",
  config = function()
    require("conform").setup({
      formatters_by_ft = {
        lua = {
          "stylua"
        },
        python = {
          "black"
        },
        terraform = {
          "terraform_fmt"
        },
        javascript = {
          {
            "prettierd",
            "prettier"
          }
        },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
    })
  end
}
