local formatting = require("null-ls").builtins.formatting
local diagnostics = require("null-ls").builtins.diagnostics
local code_actions = require("null-ls").builtins.code_actions
require("null-ls").setup {
  border = "rounded",

  on_attach = function(client, bufnr)
    -- Custom command to use null-ls as the formatter.
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
    --formatting
    formatting.djlint.with {
      filetypes = {
        "django",
        "jinja",
      },
    },
    formatting.markdownlint,
    formatting.beautysh,
    formatting.stylua,
    formatting.clang_format.with {
      filetypes = {
        "c",
        "cpp",
        "cs",
      },
    },
    formatting.gofumpt,
    formatting.prettier.with {
      filetypes = {
        "angular",
        "css",
        "flow",
        "graphql",
        "html",
        "json",
        "jsx",
        "javaScript",
        "less",
        "markdown",
        "scss",
        "typescript",
        "vue",
        "yaml",
      },
      extra_filetypes = { "toml" },
      extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" },
    },

    --diagnostics
    diagnostics.markdownlint,
    diagnostics.djlint,
    diagnostics.shellcheck,
    diagnostics.jsonlint,
    diagnostics.zsh,

    -- code_actions
    code_actions.refactoring,
  },
}
