-- Integrate Mason with nvim lsp and null-ls
local M = {
    "williamboman/mason-lspconfig.nvim",
    enabled = true,
    dependencies = {
        "neovim/nvim-lspconfig",
        {
            "williamboman/mason.nvim",
            opts = {
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
        },
        "jay-babu/mason-null-ls.nvim",
        "hrsh7th/cmp-nvim-lsp",
        "folke/neodev.nvim",
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
    event = { "BufRead", "BufNewFile" },
}
local ensure_installed = {
    -- lsp
    "lua_ls",
    "taplo",
    "vimls",
    "pylsp",
    -- "pyright",
    "yamlls",
    "gopls",
    -- linters
    "eslint_d",
    "markdownlint",
    "djlint",
    "shellcheck",
    "jsonlint",
    -- "flake8",

    -- formatters
    "beautysh",
    -- "black",
    -- "isort",
    "prettier",
}
function M.config()
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

    local keymap = require("uts").map
    -- on_attach function to be added to each server
    local on_attach = function(client, bufnr)
        -- Buffer mappings for LSP servers
        keymap("n", "<leader>lD", "<cmd>lua vim.lsp.buf.declaration()<CR>", { desc = "Declaration", buffer = bufnr })
        keymap("n", "<leader>lI", "<cmd>lua vim.lsp.buf.implementation()<CR>",
            { desc = "Implementation", buffer = bufnr })
        keymap("n", "gk", "<cmd>lua vim.diagnostic.open_float()<CR>", { desc = "Float Diagnostics", buffer = bufnr })
        keymap("n", "<leader>lc", "<cmd>lua vim.lsp.buf.code_action()<cr>", { desc = "Code Actions", buffer = bufnr })
        keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>",
            { desc = "Diagnostics Next", buffer = bufnr })
        keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>",
            { desc = "Diagnostics Prev", buffer = bufnr })
        keymap("n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<cr>", { desc = "rename", buffer = bufnr })
        keymap("n", "<leader>ls", "<cmd>lua vim.lsp.buf.signature_help()<CR>",
            { desc = "Signature Help", buffer = bufnr })
        keymap("n", "<leader>ll", "<cmd>lua vim.diagnostic.setloclist()<CR>",
            { desc = "Diagnostics loclist", buffer = bufnr })
        keymap("n", "<leader>lq", "<cmd>TroubleToggle quickfix<cr>", { desc = "Quickfix [Trouble]", buffer = bufnr })
        keymap("n", "<leader>ld", "<cmd>TroubleToggle lsp_definitions<cr>",
            { desc = "Definition [Trouble]", buffer = bufnr })
        keymap("n", "<leader>lt", "<cmd>TroubleToggle lsp_type_definitions<cr>",
            { desc = "Type Definition [Trouble]", buffer = bufnr })
        keymap("n", "<leader>lf", "<cmd>TroubleToggle lsp_references<cr>",
            { desc = "Find references [Trouble]", buffer = bufnr })
        keymap("n", "<leader>lx", "<cmd>TroubleToggle document_diagnostics<cr>",
            { desc = "Error List [Trouble]", buffer = bufnr })
        keymap({ "n", "i" }, "<c-f>", vim.lsp.buf.format, { desc = "format code", buffer = bufnr })
    end

    --setup neodev
    require("neodev").setup {
        library = {
            enabled = true, -- when not enabled, neodev will not change any settings to the LSP server
            -- these settings will be used for your Neovim config directory
            runtime = true, -- runtime path
            types = true,   -- full signature, docs and completion of vim.api, vim.treesitter, vim.lsp and others
            plugins = { "nvim-treesitter", "plenary.nvim", "telescope.nvim", "neotest" },
        },
        setup_jsonls = true, -- configures jsonls to provide completion for project specific .luarc.json files
        -- for your Neovim config directory, the config.library settings will be used as is
        -- for plugin directories (root_dirs having a /lua directory), config.library.plugins will be disabled
        -- for any other directory, config.library.enabled will be set to false
        override = function(root_dir, options) end,
        lspconfig = true,
        pathStrict = true,
    }

    -- setup lsp servers
    require("mason-lspconfig").setup {}
    require("mason-lspconfig").setup_handlers {
        function(server_name)
            require("lspconfig")[server_name].setup {
                on_attach = on_attach,
                capabilities = capabilities,
            }
        end,
        -- ["pyright"] = function()
        --     require("lspconfig").pyright.setup {
        --         on_attach = on_attach,
        --         settings = {
        --             python = {
        --                 analysis = {
        --                     autoImportCompletion = true,
        --                     autoSearchPaths = true,
        --                     diagnosticMode = 'openFilesOnly',
        --                     useLibraryCodeForTypes = true,
        --                     typeCheckingMode = 'basic'
        --                 }
        --             }
        --         },
        --         capabilities = capabilities
        --     }
        -- end,
        ["pylsp"] = function()
            require("lspconfig").pylsp.setup {
                on_attach = on_attach,
                settings = {
                    pylsp = {
                        plugins = {
                            -- formatter options
                            black = { enabled = true, line_length = 120, cache_config = true },
                            autopep8 = { enabled = false },
                            yapf = { enabled = false },
                            -- linter options
                            pylint = { enabled = true, executable = "pylint" },
                            -- ruff = { enabled = true, extendSelect = "I" },
                            pyflakes = { enabled = false },
                            pycodestyle = { enabled = false },
                            pydocstyle = { enabled = false },
                            -- type checker
                            mypy = { enabled = true, },
                            -- auto-completion options
                            jedi_completion = { fuzzy = true },
                            -- import sorting
                            isort = { enabled = true },
                            rope = { enabled = true }
                        },
                    },
                },
                flags = {
                    debounce_text_changes = 200,
                },
                capabilities = capabilities,
            }
        end,
        ["lua_ls"] = function()
            require("lspconfig").lua_ls.setup {
                on_attach = on_attach,
                settings = {
                    Lua = {
                        format = {
                            enable = true,
                            -- Put format options here
                            -- NOTE: the value should be String!
                            defaultConfig = {
                                indent_style = "space",
                                indent_size = "4",
                            },
                        },
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
                capabilities = capabilities,
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
    require("mason-null-ls").setup { ensure_installed = ensure_installed }

    local none_ls = require "null-ls"
    none_ls.setup {
        border = "rounded",

        sources = {
            --formatting
            none_ls.builtins.formatting.prettier.with {
                extra_filetypes = { "toml" },
                extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" },
            },
            none_ls.builtins.formatting.djlint,
            none_ls.builtins.formatting.markdownlint,
            none_ls.builtins.formatting.beautysh,
            -- none_ls.builtins.formatting.black,
            -- none_ls.builtins.formatting.isort,
            --diagnostics
            none_ls.builtins.diagnostics.eslint_d,
            none_ls.builtins.diagnostics.markdownlint,
            none_ls.builtins.diagnostics.djlint,
            -- none_ls.builtins.diagnostics.flake8,
            none_ls.builtins.diagnostics.shellcheck,
            none_ls.builtins.diagnostics.jsonlint,
        },
    }

    -- setup lsp diagnostic signs
    vim.fn.sign_define("DiagnosticSignError", { texthl = "DiagnosticSignError", text = " ", numhl = "" })
    vim.fn.sign_define("DiagnosticSignWarn", { texthl = "DiagnosticSignWarn", text = " ", numhl = "" })
    vim.fn.sign_define("DiagnosticSignHint", { texthl = "DiagnosticSignHint", text = "󰌵 ", numhl = "" })
    vim.fn.sign_define("DiagnosticSignInfo", { texthl = "DiagnosticSignInfo", text = " ", numhl = "" })
end

return M
