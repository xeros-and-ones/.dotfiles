dofile(vim.g.base46_cache .. "lsp")
require "nvchad.lsp"

local map = function(mode, lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end

local utils = require "core.utils"
local cmp_nvim_lsp = require "cmp_nvim_lsp"

local signs = { Error = "", Warn = "", Hint = "󰌵", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.diagnostic.config {
  -- Enable virtual text
  virtual_text = false,
  -- show signs
  signs = {
    active = signs,
  },
  update_in_insert = true,
  underline = true,
  severity_sort = true,
  float = {
    focusable = false,
    style = "minimal",
    border = "rounded",
    source = "always",
    header = "",
    prefix = "",
  },
}
require("lspconfig.ui.windows").default_options.border = "rounded"

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = "rounded",
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
  border = "rounded",
})

-- LSP Keymapping
local function lsp_keymaps(bufnr)
  map("n", "gk", "<cmd>Lspsaga show_line_diagnostics<CR>", { desc = "Line Diagnostic", buffer = bufnr })
  map("n", "gp", "<cmd>Lspsaga peek_definition<CR>", { desc = "Peek_Definition", buffer = bufnr })
  map("n", "gm", "<cmd>Telescope lsp_implementations<CR>", { desc = "LSP implementation", buffer = bufnr })
  map({ "n", "v" }, "<leader>la", "<cmd>Lspsaga code_action<cr>", { desc = "Code Action", buffer = bufnr })
  map("n", "<leader>ld", "<cmd>Lspsaga goto_definition<cr>", { desc = "Goto_Definition", buffer = bufnr })
  map("n", "<leader>lO", "<cmd>Lspsaga outline<cr>", { desc = "Code Outline", buffer = bufnr })
  map("n", "<leader>li", "<cmd>Lspsaga incoming_calls<cr>", { desc = "Incoming Calls", buffer = bufnr })
  map("n", "<leader>lo", "<cmd>Lspsaga outgoing_calls<cr>", { desc = "Outgoing Calls" })
  map("n", "]d", "<cmd>Lspsaga diagnostic_jump_next<cr>", { desc = "Next Diagnostic", buffer = bufnr })
  map("n", "[d", "<cmd>Lspsaga diagnostic_jump_prev<cr>", { desc = "Prev Diagnostic", buffer = bufnr })
  map("n", "<leader>lR", "<cmd>LspRestart<cr>", { desc = "Restart LSP", buffer = bufnr })
  map("n", "<leader>lr", "<cmd>Lspsaga rename<cr>", { desc = "Rename", buffer = bufnr })
  map("n", "<leader>lF", "<cmd>Lspsaga finder tyd+ref+imp+def<cr>", { desc = "LspSaga Finder", buffer = bufnr })
  map("n", "<leader>lq", "<cmd>TroubleToggle quickfix<cr>", { desc = "Quickfix [Trouble]", buffer = bufnr })
  if vim.lsp.inlay_hint then
    map("n", "<leader>lh", "<cmd>lua vim.lsp.inlay_hint(0, nil)<cr>", { desc = "Inlay Hint", buffer = bufnr })
  end
  map("n", "<leader>lD", "<cmd>TroubleToggle lsp_definitions<cr>", { desc = "Definition [Trouble]", buffer = bufnr })
  map(
    "n",
    "<leader>lf",
    "<cmd>TroubleToggle lsp_references<cr>",
    { desc = "Find references [Trouble]", buffer = bufnr }
  )
  map(
    "n",
    "<leader>lt",
    "<cmd>TroubleToggle lsp_type_definitions<cr>",
    { desc = "Type Definition [Trouble]", buffer = bufnr }
  )
  map("n", "<leader>lx", "<cmd>TroubleToggle document_diagnostics<cr>", { desc = "Buffer Diagnostics", buffer = bufnr })
  map(
    "n",
    "<leader>lw",
    "<cmd>TroubleToggle workspace_diagnostics<cr>",
    { desc = "Workspace Diagnostics", buffer = bufnr }
  )
  map("n", "go", "<cmd>Telescope lsp_document_symbols<cr>", { desc = "Buffer Symbols" })
  map("n", "gO", "<cmd>Telescope lsp_workspace_symbols<cr>", { desc = "Workspace Symbols" })

  map("n", "<leader>ls", function()
    vim.lsp.buf.signature_help()
  end, { desc = "LSP signature help", buffer = bufnr })

  -- map("n", "gr", function()
  -- 	vim.lsp.buf.references()
  -- end, { desc = "LSP references", buffer = bufnr })

  map("n", "gD", function()
    vim.lsp.buf.declaration()
  end, { "LSP declaration", buffer = bufnr })
end
---------------------------------------------------------
local on_attach = function(client, bufnr)
  client.server_capabilities.documentFormattingProvider = false
  client.server_capabilities.documentRangeFormattingProvider = false
  lsp_keymaps(bufnr)
  if client.server_capabilities.signatureHelpProvider then
    require("nvchad.signature").setup(client)
  end

  if not utils.load_config().ui.lsp_semantic_tokens and client.supports_method "textDocument/semanticTokens" then
    client.server_capabilities.semanticTokensProvider = nil
  end
end
--------------------------------------------------------
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = cmp_nvim_lsp.default_capabilities()
capabilities.textDocument.completion.completionItem = {
  documentationFormat = { "markdown", "plaintext" },
  snippetSupport = true,
  preselectSupport = true,
  insertReplaceSupport = true,
  labelDetailsSupport = true,
  deprecatedSupport = true,
  commitCharactersSupport = true,
  tagSupport = { valueSet = { 1 } },
  resolveSupport = {
    properties = {
      "documentation",
      "detail",
      "additionalTextEdits",
    },
  },
}
capabilities["textDocument"]["foldingRange"] = {
  dynamicRegistration = false,
  lineFoldingOnly = true,
}

---------------------------------------------------------

local mason_lspconfig = require "mason-lspconfig"

local disabled_servers = {
  "pyright",
}

mason_lspconfig.setup_handlers {
  function(server_name)
    for _, name in pairs(disabled_servers) do
      if name == server_name then
        return
      end
    end
    local opts = {
      on_attach = on_attach,
      capabilities = capabilities,
      single_file_support = true,
    }

    local require_ok, server = pcall(require, "custom.configs.lsp_servers." .. server_name)
    if require_ok then
      opts = vim.tbl_deep_extend("force", server, opts)
    end

    require("lspconfig")[server_name].setup(opts)
  end,
}
