local M = {
    "neovim/nvim-lspconfig",
    enabled = true,
    dependencies = {
        "williamboman/mason-lspconfig.nvim",
        {
            "mrcjkb/rustaceanvim",
            version = "^3", -- Recommended
            init = function()
                -- Configure rustaceanvim here
                vim.g.rustaceanvim = {}
            end,
            ft = { "rust" },
        },
        {
            "pmizio/typescript-tools.nvim",
            opts = {
                settings = {
                    tsserver_file_preferences = {
                        includeInlayParameterNameHints = "all",
                        includeCompletionsForModuleExports = true,
                        quotePreference = "auto",
                    },
                },
            },
        },
    },
    event = { "BufRead", "BufNewFile" },
}

local signs = { Error = "", Warn = "", Hint = "󰌵", Info = "" }
for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.diagnostic.config {
    signs = {
        active = signs,
    },
    -- disable virtual text
    virtual_text = false,
    update_in_insert = true,
    underline = true,
    severity_sort = true,
    float = {
        focusable = false,
        -- style = "minimal",
        format = function(diagnostic)
            return string.format(
                "%s (%s) [%s]",
                diagnostic.message,
                diagnostic.source,
                diagnostic.code or diagnostic.user_data.lsp.code
            )
        end,
        signs = true,
        show_header = true,
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
        suffix = "",
    },
}

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "rounded",
})

----------------------------------------------------------------------------------

M.config = function()
    require("lspconfig.ui.windows").default_options.border = "rounded"

    -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
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

    -- on_attach function to be added to each server
    local on_attach = function(client, bufnr)
        LspMapping(bufnr)
    end

    ----------------------------------------------------------------
    ----------------------------------------------------------------
    ----------------------------------------------------------------
    ----------------------------------------------------------------
    ----------------------------------------------------------------
    --- Setup LSP Servers ------------------------------------------

    local mason_lspconfig = require "mason-lspconfig"
    mason_lspconfig.setup()
    mason_lspconfig.setup_handlers {
        function(server_name)
            local opts = {
                on_attach = on_attach,
                capabilities = capabilities,
                single_file_support = true,
            }

            local require_ok, server = pcall(require, "Plugins.lsp_servers." .. server_name)
            if require_ok then
                opts = vim.tbl_deep_extend("force", server, opts)
            end

            require("lspconfig")[server_name].setup(opts)
        end,
    }
end

return M
