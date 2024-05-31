local capabilities = vim.lsp.protocol.make_client_capabilities()
local icons = require("helpers.icons")
local mason_lspconfig = require("mason-lspconfig")
local schemastore = require("schemastore")
local telescope_builtin = require("telescope.builtin")

local M = {}

function M.setup()
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
    nmap("<leader>D", vim.lsp.buf.type_definition)
    nmap("<leader>K", vim.lsp.buf.hover)

    nmap("<leader>ga", vim.lsp.buf.code_action)
    nmap("<leader>gr", telescope_builtin.lsp_references)
    nmap("<leader>gI", telescope_builtin.lsp_implementations)
    nmap('<leader>ds', telescope_builtin.lsp_document_symbols)
    nmap('<leader>ws', telescope_builtin.lsp_dynamic_workspace_symbols)

    -- Lesser used LSP functionality
    nmap("<C-k>", vim.lsp.buf.signature_help)
    nmap("gD", vim.lsp.buf.declaration)
    nmap("<leader>wa", vim.lsp.buf.add_workspace_folder)
    nmap("<leader>wr", vim.lsp.buf.remove_workspace_folder)
    nmap("<leader>wl", function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end)

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
      vim.lsp.buf.format()
    end, {})
  end

  -- Servers which don't need any configuring here
  local unconfigured = {
    cssls       = true,
    dockerls    = true,
    eslint      = true, -- move this?
    gopls       = true,
    groovyls    = true,
    html        = true,
    pyright     = true,
    terraformls = true,
    tflint      = true, -- move this?
    tsserver    = true,
  }
  -- Servers which ARE configured
  local servers = {
    jsonls = {
      json = {
        schemas = schemastore.json.schemas()
      }
    },
    yamlls = {
      yaml = {
        customTags = {
          "!reference sequence", -- This was extremely hard to figure out
          "!ImportValue"
        },
        schemas = schemastore.yaml.schemas()
      }
    },
    lua_ls = {
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
  for k, v in pairs(unconfigured) do
    if v == true then
      servers[k] = {}
    end
  end

  -- Enable the following language servers

  -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

  -- Ensure the servers above are installed
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
  local signs = icons.diagnostics
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

M.setup()

return M
