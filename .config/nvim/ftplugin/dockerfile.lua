vim.api.nvim_create_autocmd(
  "BufWritePost", {
    command = [[ :!docker run --rm -i hadolint/hadolint < % ]]
  }
)
