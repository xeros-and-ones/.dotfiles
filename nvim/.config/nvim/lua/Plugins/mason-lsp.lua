-- Integrate Mason with nvim lsp and null-ls
local M = {
  "williamboman/mason-lspconfig.nvim",
  enabled = true,
  dependencies = {
    "neovim/nvim-lspconfig",
    "williamboman/mason.nvim",
    "jay-babu/mason-null-ls.nvim",
    "hrsh7th/cmp-nvim-lsp",
    "simrat39/rust-tools.nvim",
    {
      "nvimtools/none-ls.nvim",
      event = "BufReadPre",
      commit = "ae339f45590cc421a68de885fc5a3261cc247362",
      dependencies = {
        {
          "nvim-lua/plenary.nvim",
        },
      },
    },
  },
  event = { "BufReadPre", "BufNewFile" },
}

function M.config()
  -- setup mason for tool installation
  require("mason").setup {
    ui = {
      border = "rounded",
      icons = {
        package_installed = "✓",
        package_uninstalled = "✗",
        package_pending = "⟳",
      },
    },
    log_level = vim.log.levels.INFO,
    max_concurrent_installers = 4,
  }
  require("lspconfig.ui.windows").default_options.border = "rounded"
  -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
  capabilities["textDocument"]["foldingRange"] = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
  }

  -- settings for specific lsp servers
  local runtime_path = vim.split(package.path, ";")
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")

  local function lsp_keymaps(bufnr)
    local keymap = vim.api.nvim_buf_set_keymap
    keymap(
      bufnr,
      "n",
      "<leader>lD",
      "<cmd>lua vim.lsp.buf.declaration()<CR>",
      { desc = "Declaration", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>lI",
      "<cmd>lua vim.lsp.buf.implementation()<CR>",
      { desc = "Implementation", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "gk",
      "<cmd>lua vim.diagnostic.open_float()<CR>",
      { desc = "Float Diagnostics", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>lc",
      "<cmd>lua vim.lsp.buf.code_action()<cr>",
      { desc = "Code Actions", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "]d",
      "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>",
      { desc = "Diagnostics Next", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "[d",
      "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>",
      { desc = "Diagnostics Prev", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>lr",
      "<cmd>lua vim.lsp.buf.rename()<cr>",
      { desc = "rename", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>ls",
      "<cmd>lua vim.lsp.buf.signature_help()<CR>",
      { desc = "Signature Help", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>ll",
      "<cmd>lua vim.diagnostic.setloclist()<CR>",
      { desc = "Diagnostics loclist", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>lq",
      "<cmd>TroubleToggle quickfix<cr>",
      { desc = "Quickfix [Trouble]", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>ld",
      "<cmd>TroubleToggle lsp_definitions<cr>",
      { desc = "Definition [Trouble]", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>lt",
      "<cmd>TroubleToggle lsp_type_definitions<cr>",
      { desc = "Type Definition [Trouble]", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>lf",
      "<cmd>TroubleToggle lsp_references<cr>",
      { desc = "Find references [Trouble]", noremap = true, silent = true }
    )
    keymap(
      bufnr,
      "n",
      "<leader>lx",
      "<cmd>TroubleToggle<cr>",
      { desc = "Error List [Trouble]", noremap = true, silent = true }
    )
  end

  local signs = {
    { name = "DiagnosticSignError", text = "" },
    { name = "DiagnosticSignWarn", text = "" },
    { name = "DiagnosticSignHint", text = "" },
    { name = "DiagnosticSignInfo", text = "" },
  }

  for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
  end

  vim.diagnostic.config {
    -- disable virtual text
    virtual_text = false,
    virtual_lines = false,
    { highlight_whole_line = false },
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
      suffix = "",
    },
  }
  -- on_attach function to be added to each server
  local on_attach = function(client, bufnr)
    -- map buffer local keys once lsp is attached
    lsp_keymaps(bufnr)
  end

  -- setup lsp servers
  require("mason-lspconfig").setup {
    ensure_installed = {
      -- lsp
      "lua_ls",
      "pyright", -- for static type checking only
      "taplo",
      "vimls",
      "yamlls",
      "gopls",
    },
  }
  require("mason-lspconfig").setup_handlers {
    function(server_name)
      require("lspconfig")[server_name].setup {
        on_attach = on_attach,
        capabilities = capabilities,
      }
    end,
    ["pyright"] = function()
      require("lspconfig").pyright.setup {
        capabilities = capabilities,
        on_attach = on_attach,
        dibleLanguageServices = false,
        disableOrganizeImports = false,
        python = {
          analysis = {
            autoImportCompletions = true,
            autoSearchPaths = true,
            useLibraryCodeForTypes = true,
            typeCheckingMode = "basic",
            diagnosticMode = "openFilesOnly",
            --       diagnosticSeverityOverrides = {
            --         reportGeneralTypeIssues = "information", -- broken on pyrites end
            --       },
          },
        },
      }
    end,
    ["lua_ls"] = function()
      require("lspconfig").lua_ls.setup {
        capabilities = capabilities,
        settings = {
          Lua = {
            telemetry = { enable = false },
            runtime = {
              version = "LuaJIT",
              path = runtime_path,
            },
            diagnostics = {
              -- Get the language server to recognize the `vim` global
              globals = { "vim" },
            },
            workspace = {
              checkThirdParty = false,
              library = {
                -- Make the server aware of Neovim runtime files
                vim.fn.expand "$VIMRUNTIME/lua",
                vim.fn.stdpath "config" .. "/lua",
              },
            },
            completion = {
              callSnippet = "Replace",
            },
          },
        },
      }
    end,
    ["rust_analyzer"] = function() -- dont autosetup rust_analyzer; use rust-tools instead
      require("rust-tools").setup {
        tools = {
          inlay_hints = { auto = true },
          hover_actions = { border = "solid" },
          executor = require("rust-tools/executors").toggleterm,
        },
        server = {
          on_attach = on_attach,
          standalone = true,
          capabilities = capabilities,
          checkOnSave = {
            allFeatures = true,
            overrideCommand = {
              "cargo",
              "clippy",
              "--workspace",
              "--message-format=json",
              "--all-targets",
              "--all-features",
            },
          },
        },
      }
    end,
  }

  -- setup null-ls
  require("mason-null-ls").setup {
    ensure_installed = {
      -- linters
      "eslint_d",
      "markdownlint",
      "flake8",
      "luacheck",
      "djlint",
      "shellcheck",
      "jsonlint",

      -- formatters
      "black",
      "isort",
      "beautysh",
      "prettier",
      "rustfmt",
      "stylua",
    }, -- lsp, linter, formatter
  }

  local none_ls = require "null-ls"
  local code_actions = none_ls.builtins.code_actions
  local completion = none_ls.builtins.completion
  none_ls.setup {
    border = "rounded",
    on_attach = function(client, bufnr)
      --  Custom command to use null-ls as the formatter.
      local format_cmd = function(input)
        vim.lsp.buf.format {
          id = client.id,
          timeout_ms = 5000,
          async = input.bang,
        }
      end

      local bufcmd = vim.api.nvim_buf_create_user_command
      bufcmd(bufnr, "NullFormat", format_cmd, {
        bang = true,
        range = true,
      })

      -- format on save
      if client.supports_method "textDocument/formatting" then
        local format_group = vim.api.nvim_create_augroup("autoformat", { clear = true })
        vim.api.nvim_create_autocmd("BufWritePre", {
          group = format_group,
          buffer = bufnr,
          callback = function()
            vim.cmd "NullFormat"
          end,
        })
      end
    end,

    sources = {
      --completion
      completion.spell,
      --formatting
      none_ls.builtins.formatting.prettier.with {
        extra_filetypes = { "toml" },
        extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" },
      },
      none_ls.builtins.formatting.black.with { extra_args = { "--fast" } },
      none_ls.builtins.formatting.isort,
      none_ls.builtins.formatting.stylua,
      none_ls.builtins.formatting.djlint,
      none_ls.builtins.formatting.markdownlint,
      none_ls.builtins.formatting.beautysh,
      none_ls.builtins.formatting.rustfmt,
      --diagnostics
      none_ls.builtins.diagnostics.luacheck,
      none_ls.builtins.diagnostics.flake8.with { extra_args = { "--max-line-length", "88" } },
      none_ls.builtins.diagnostics.eslint_d,
      none_ls.builtins.diagnostics.markdownlint,
      none_ls.builtins.diagnostics.djlint,
      none_ls.builtins.diagnostics.shellcheck,
      none_ls.builtins.diagnostics.jsonlint,
      --code_actions
      code_actions.refactoring,
    },
  }

  -- setup lsp diagnostic signs
  vim.fn.sign_define("DiagnosticSignError", { texthl = "DiagnosticSignError", text = " ", numhl = "" })
  vim.fn.sign_define("DiagnosticSignWarn", { texthl = "DiagnosticSignWarn", text = " ", numhl = "" })
  vim.fn.sign_define("DiagnosticSignHint", { texthl = "DiagnosticSignHint", text = "󰌵 ", numhl = "" })
  vim.fn.sign_define("DiagnosticSignInfo", { texthl = "DiagnosticSignInfo", text = " ", numhl = "" })
end

return M
