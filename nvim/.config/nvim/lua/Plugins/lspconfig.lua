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

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "rounded",
    focusable = false,
    relative = "cursor",
})

----------------------------------------------------------------------------------
local LSP_mappings = {
    n = {
        ["gD"] = {
            function()
                vim.lsp.buf.declaration()
            end,
            "LSP declaration",
        },
        ["gk"] = { ":Lspsaga show_line_diagnostics<CR>", "Line Diagnostic" },
        -- ["gk"] = {
        --     function()
        --         vim.diagnostic.open_float()
        --     end,
        --     "Floating diagnostic",
        -- },

        -- ["gh"] = {
        --     function()
        --         vim.lsp.buf.hover()
        --     end,
        --     "LSP hover",
        -- },
        ["gh"] = { ":Lspsaga hover_doc<CR>", "LSP hover" },
        ["gp"] = { ":Lspsaga peek_definition<CR>", "Peek_Definition" },

        ["gi"] = { ":Telescope lsp_implementations<CR>", "LSP implementation" },

        ["<leader>ls"] = {
            function()
                vim.lsp.buf.signature_help()
            end,
            "LSP signature help",
        },

        -- ["gd"] = {
        --     function()
        --         vim.lsp.buf.type_definition()
        --     end,
        --     "LSP definition type",
        -- },

        ["gr"] = {
            function()
                vim.lsp.buf.references()
            end,
            "LSP references",
        },

        ["[d"] = {
            function()
                vim.diagnostic.goto_prev { buffer = 0 }
            end,
            "Diagnostics Prev",
        },

        ["]d"] = {
            function()
                vim.diagnostic.goto_next { buffer = 0 }
            end,
            "Diagnostics Next",
        },

        ["<leader>la"] = { "<cmd>Lspsaga code_action<cr>", "Code Action" },
        ["<leader>ld"] = { "<cmd>Lspsaga goto_definition<cr>", "Goto_Definition" },
        ["<leader>lO"] = { "<cmd>Lspsaga outline<cr>", "Code Outline" },
        ["<leader>li"] = { "<cmd>Lspsaga incoming_calls<cr>", "Incoming Calls" },
        ["<leader>lo"] = { "<cmd>Lspsaga outgoing_calls<cr>", "Outgoing Calls" },
        ["<leader>lj"] = {
            "<cmd>Lspsaga diagnostic_jump_next<cr>",
            "Next Diagnostic",
        },
        ["<leader>lk"] = {
            "<cmd>Lspsaga diagnostic_jump_prev<cr>",
            "Prev Diagnostic",
        },
        ["<leader>lR"] = { "<cmd>LspRestart<cr>", "Restart LSP" },
        ["<leader>lr"] = { "<cmd>Lspsaga rename<cr>", "Rename" },
        ["<leader>lF"] = { "<cmd>Lspsaga finder tyd+ref+imp+def<cr>", "LspSaga Finder" },
        ["<leader>lq"] = { "<cmd>TroubleToggle quickfix<cr>", "Quickfix [Trouble]" },
        ["<leader>lD"] = { "<cmd>TroubleToggle lsp_definitions<cr>", "Definition [Trouble]" },
        ["<leader>lf"] = { "<cmd>TroubleToggle lsp_references<cr>", "Find references [Trouble]" },
        ["<leader>lt"] = {
            "<cmd>TroubleToggle lsp_type_definitions<cr>",
            "Type Definition [Trouble]",
        },
        ["<C-`>"] = { "<cmd>TroubleToggle<cr>", "Toggle Trouble" },
        ["<leader>lx"] = { "<cmd>TroubleToggle document_diagnostics<cr>", "Buffer Diagnostics" },
        ["<leader>lw"] = {
            "<cmd>TroubleToggle workspace_diagnostics<cr>",
            "Workspace Diagnostics",
        },
        ["go"] = { "<cmd>Telescope lsp_document_symbols<cr>", "Buffer Symbols" },
        ["gO"] = { "<cmd>Telescope lsp_workspace_symbols<cr>", "Workspace Symbols" },
        ["<leader>lh"] = { ":lua vim.lsp.inlay_hint(0, nil)<cr>", "Inlay Hint" },
    },

    v = {

        ["<leader>la"] = { "<cmd>Lspsaga code_action<cr>", "Code Action" },
    },
}
local function set_section_map(section_values, mapping_opts)
    local merge_tb = vim.tbl_deep_extend
    for mode, mode_values in pairs(section_values) do
        local default_opts = merge_tb("force", { mode = mode }, mapping_opts or {})
        for keybind, mapping_info in pairs(mode_values) do
            -- merge default + user opts
            local opts = merge_tb("force", default_opts, mapping_info.opts or {})

            mapping_info.opts, opts.mode = nil, nil
            opts.desc = mapping_info[2]

            vim.keymap.set(mode, keybind, mapping_info[1], opts)
        end
    end
end
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
        set_section_map(LSP_mappings, { buffer = bufnr, silent = true })
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
