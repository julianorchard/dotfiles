local M = {
  "neovim/nvim-lspconfig",
  dependencies = {
    {
      "williamboman/mason.nvim",
      config = true
    },
    "williamboman/mason-lspconfig.nvim",
    {
      "j-hui/fidget.nvim",
      tag = "legacy",
      opts = {}
    },
    {
      "folke/neodev.nvim",
      config = function()
        -- Setup neovim lua configuration
        require("neodev").setup()
      end
    },

    "b0o/SchemaStore.nvim"
  }
}

M.config = function()
  -- Diagnostic keymaps
  vim.keymap.set("n", "[", vim.diagnostic.goto_prev)
  vim.keymap.set("n", "]", vim.diagnostic.goto_next)
  vim.keymap.set("n", "<leader>gh", vim.diagnostic.open_float)
  vim.keymap.set("n", "<leader>gl", vim.diagnostic.setloclist)

  local on_attach = function(_, bufnr)
    local nmap = function(keys, func, desc)
      if desc then
        desc = "LSP: " .. desc
      end
      vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
    end

    -- Most useful (but I don't often get to use them)
    nmap("<leader>gc", vim.lsp.buf.rename)
    nmap("<leader>gd", vim.lsp.buf.definition)
    nmap('<leader>D', vim.lsp.buf.type_definition)
    nmap('K', vim.lsp.buf.hover)

    nmap("<leader>ga", vim.lsp.buf.code_action)
    nmap("<leader>gr", require("telescope.builtin").lsp_references)
    nmap("<leader>gI", require("telescope.builtin").lsp_implementations)
    nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols)
    nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols)

    -- Lesser used LSP functionality
    nmap('<C-k>', vim.lsp.buf.signature_help)
    nmap('gD', vim.lsp.buf.declaration)
    nmap('<leader>wa', vim.lsp.buf.add_workspace_folder)
    nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder)
    nmap('<leader>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end)

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
      vim.lsp.buf.format()
    end)
  end

  -- Enable the following language servers
  local servers = {
    pyright  = {},
    gopls    = {},
    tsserver = {},
    jsonls   = {
      json = {
        schemas = require("schemastore").json.schemas()
      }
    },
    yamlls   = {
      yaml = {
        customTags = {
          "!reference sequence", -- This was extremely hard to figure out
          "!ImportValue"
        },
        schemas = require("schemastore").yaml.schemas()
      }
    },
    lua_ls   = {
      Lua = {
        diagnostics = {
          globals = {
            -- Neovim
            "vim",
            -- Awesome
            "awesome",
            "client",
            "screen",
            "tag",
          }
        },
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
      },
    },
  }

  -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

  -- Ensure the servers above are installed
  local mason_lspconfig = require("mason-lspconfig")
  mason_lspconfig.setup({
    ensure_installed = vim.tbl_keys(servers),
  })
  mason_lspconfig.setup_handlers({
    function(server_name)
      require("lspconfig")[server_name].setup({
        capabilities = capabilities,
        on_attach = on_attach,
        settings = servers[server_name],
        filetypes = (servers[server_name] or {}).filetypes,
      })
    end
  })

  -- Disable virtual_text since it's redundant due to lsp_lines.
  vim.diagnostic.config({
    virtual_text = false,
  })

  -- Custom signs
  local signs = require("helpers.icons").diagnostics
  for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
  end

  -- json (includes: tfstate)
  vim.api.nvim_create_autocmd(
    { "BufNewFile", "BufRead" }, {
      pattern = { "*.tfstate" },
      command = [[ :set filetype=json]]
    }
  )
  -- html (includes: njk, )
  vim.api.nvim_create_autocmd(
    { "BufNewFile", "BufRead" }, {
      pattern = { "*.njk" },
      command = [[ :set filetype=html]]
    }
  )

  vim.filetype.add({
    extension = {
      jenkins = "groovy"
    }
  })
end

return M
