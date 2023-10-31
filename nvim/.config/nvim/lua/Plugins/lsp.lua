-- Integrate Mason with nvim lsp and null-ls
local M = {
    "neovim/nvim-lspconfig",
    enabled = true,
    dependencies = {
        "b0o/schemastore.nvim",
        {
            "williamboman/mason.nvim",
            event = "VimEnter",
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
            },
        },
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
    event = { "BufRead", "BufNewFile" },
}
local ensure_installed = {
    -- lsp ---------------------------------------
    "lua_ls",
    "taplo",
    "vimls",
    "cssls",
    "html-lsp",
    "python-lsp-server",
    -- "pyright",
    "tsserver",
    "yamlls",
    "jsonls",
    "gopls",

    -- debuggers ---------------------------------
    "debugpy",

    -- linters -----------------------------------
    "markdownlint",
    "djlint",
    "shellcheck",
    "jsonlint",
    -- "flake8",

    -- formatters -------------------------------
    "beautysh",
    "stylua",
    -- "black",
    -- "isort",
    "prettier",
}
function M.config()
    require("lspconfig.ui.windows").default_options.border = "rounded"
    -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    capabilities["textDocument"]["foldingRange"] = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
    }

    -- on_attach function to be added to each server
    local keymap = require("uts").map
    local on_attach = function(client, bufnr)
        --
        -- Buffer mappings for LSP servers
        keymap("n", "<leader>lD", "<cmd>lua vim.lsp.buf.declaration()<CR>", { desc = "Declaration", buffer = bufnr })
        keymap(
            "n",
            "<leader>lI",
            "<cmd>lua vim.lsp.buf.implementation()<CR>",
            { desc = "Implementation", buffer = bufnr }
        )
        keymap("n", "gk", "<cmd>lua vim.diagnostic.open_float()<CR>", { desc = "Float Diagnostics", buffer = bufnr })
        keymap(
            { "n", "v" },
            "<leader>lc",
            "<cmd>lua vim.lsp.buf.code_action()<cr>",
            { desc = "Code Actions", buffer = bufnr }
        )
        keymap(
            "n",
            "]d",
            "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>",
            { desc = "Diagnostics Next", buffer = bufnr }
        )
        keymap(
            "n",
            "[d",
            "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>",
            { desc = "Diagnostics Prev", buffer = bufnr }
        )
        keymap("n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<cr>", { desc = "rename", buffer = bufnr })
        keymap(
            "n",
            "<leader>ls",
            "<cmd>lua vim.lsp.buf.signature_help()<CR>",
            { desc = "Signature Help", buffer = bufnr }
        )
        keymap(
            "n",
            "<leader>ll",
            "<cmd>lua vim.diagnostic.setloclist()<CR>",
            { desc = "Diagnostics loclist", buffer = bufnr }
        )
        keymap("n", "<leader>lq", "<cmd>TroubleToggle quickfix<cr>", { desc = "Quickfix [Trouble]", buffer = bufnr })
        keymap(
            "n",
            "<leader>ld",
            "<cmd>TroubleToggle lsp_definitions<cr>",
            { desc = "Definition [Trouble]", buffer = bufnr }
        )
        keymap(
            "n",
            "<leader>lt",
            "<cmd>TroubleToggle lsp_type_definitions<cr>",
            { desc = "Type Definition [Trouble]", buffer = bufnr }
        )
        keymap(
            "n",
            "<leader>lf",
            "<cmd>TroubleToggle lsp_references<cr>",
            { desc = "Find references [Trouble]", buffer = bufnr }
        )
        keymap(
            "n",
            "<leader>lx",
            "<cmd>TroubleToggle document_diagnostics<cr>",
            { desc = "Error List [Trouble]", buffer = bufnr }
        )
        --
        --
        --
    end
    keymap({ "n", "i" }, "<c-a>", "<cmd>lua vim.lsp.buf.format { async = true }<cr>", { desc = "format code" })

    ------------------------ Pyright-lsp-server Config ------------------------------
    -- setup lsp servers
    -- require("lspconfig").pyright.setup {
    --   on_attach = on_attach,
    --   settings = {
    --     python = {
    --       analysis = {
    --         autoImportCompletion = true,
    --         autoSearchPaths = true,
    --         diagnosticMode = "openFilesOnly",
    --         useLibraryCodeForTypes = true,
    --         typeCheckingMode = "basic",
    --       },
    --     },
    --   },
    --   flags = {
    --     debounce_text_changes = 200,
    --   },
    --   capabilities = capabilities,
    -- }

    ------------------------ json-lsp-server Config ------------------------------
    require("lspconfig").jsonls.setup {
        on_attach = on_attach,
        settings = {
            json = {
                schemas = require("schemastore").json.schemas(),
                validate = { enable = true },
            },
        },
        capabilities = capabilities,
    }

    ------------------------ yaml-lsp-server Config ------------------------------
    require("lspconfig").yamlls.setup {
        on_attach = on_attach,
        settings = {
            yaml = {
                schemaStore = {
                    -- You must disable built-in schemaStore support if you want to use
                    -- this plugin and its advanced options like `ignore`.
                    enable = false,
                    -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
                    url = "",
                },
                schemas = require("schemastore").yaml.schemas(),
                format = { enabled = false },
                validate = false,
                completion = true,
                hover = true,
            },
        },
        capabilities = capabilities,
    }

    ------------------------ html-lsp-server Config ------------------------------
    require("lspconfig").html.setup {
        on_attach = on_attach,
        init_options = {
            provideFormatter = false,
        },
        settings = {
            html = {
                format = {
                    enable = false,
                },
            },
        },
        filetypes = { "html", "htmldjango" },
        flags = {
            debounce_text_changes = 200,
            allow_incremental_sync = true,
        },
        capabilities = capabilities,
    }

    ------------------------ python-lsp-server Config ------------------------------
    local venv_path = os.getenv "VIRTUAL_ENV"
    local py_path = nil
    -- decide which python executable to use for mypy
    if venv_path ~= nil then
        py_path = venv_path .. "/bin/python3"
    else
        py_path = vim.g.python3_host_prog
    end
    require("lspconfig").pylsp.setup {
        on_attach = on_attach,
        settings = {
            pylsp = {
                plugins = {
                    configurationSources = { "pycodestyle" },
                    -- formatter options
                    black = { enabled = true, line_length = 100, cache_config = true },
                    autopep8 = { enabled = false },
                    yapf = { enabled = false },
                    -- linter options
                    pylint = { enabled = false, executable = "pylint" },
                    ruff = {
                        enabled = true,
                        select = { "E4", "E7", "E9", "F" },
                        format = "I",
                        lineLength = 100,
                    },
                    pyflakes = { enabled = false },
                    pycodestyle = { enabled = false },
                    pydocstyle = { enabled = false },
                    mccabe = { enabled = false },
                    -- type checker
                    pylsp_mypy = {
                        enabled = true,
                        overrides = { "--python-executable", py_path, true },
                        report_progress = true,
                        live_mode = false,
                    },
                    -- auto-completion options
                    jedi_completion = {
                        enabled = true,
                        fuzzy = true,
                        include_params = false,
                    },
                    -- import sorting
                    pyls_isort = { enabled = true },
                },
            },
        },
        flags = {
            debounce_text_changes = 200,
            allow_incremental_sync = true,
        },
        capabilities = capabilities,
    }

    ---------------------- Typescript-lsp ----------------------------------------
    require("lspconfig").tsserver.setup {
        on_attach = on_attach,
        javascript = {
            inlayHints = {
                includeInlayEnumMemberValueHints = true,
                includeInlayFunctionLikeReturnTypeHints = true,
                includeInlayFunctionParameterTypeHints = true,
                includeInlayParameterNameHints = "all", -- 'none' | 'literals' | 'all';
                includeInlayParameterNameHintsWhenArgumentMatchesName = true,
                includeInlayPropertyDeclarationTypeHints = true,
                includeInlayVariableTypeHints = true,
            },
        },
        typescript = {
            inlayHints = {
                includeInlayEnumMemberValueHints = true,
                includeInlayFunctionLikeReturnTypeHints = true,
                includeInlayFunctionParameterTypeHints = true,
                includeInlayParameterNameHints = "all", -- 'none' | 'literals' | 'all';
                includeInlayParameterNameHintsWhenArgumentMatchesName = true,
                includeInlayPropertyDeclarationTypeHints = true,
                includeInlayVariableTypeHints = true,
            },
        },
        capabilities = capabilities,
    }
    ---------------------- Lua-language-server Config ----------------------------
    local lua_rtp = vim.split(package.path, ";")
    table.insert(lua_rtp, "lua/?.lua")
    table.insert(lua_rtp, "lua/?/init.lua")
    require("lspconfig").lua_ls.setup {
        on_attach = on_attach,
        lua = {
            hint = { enable = true },
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = "LuaJIT",
                -- Setup your lua path
                path = lua_rtp,
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { "vim" },
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = vim.api.nvim_get_runtime_file("", true),
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
        -- flags = {
        --     -- allow_incremental_sync = true,
        --     debounce_text_changes = 200,
        -- },
        capabilities = capabilities,
    }

    --------------------- Rust-Tools Config --------------------------------------
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

    --------------------- css-lsp Config --------------------------------------
    require("lspconfig").cssls.setup {
        on_attach = on_attach,
        css = {
            validate = true,
            lint = {
                unknownAtRules = "ignore",
            },
        },
        scss = {
            validate = true,
            lint = {
                unknownAtRules = "ignore",
            },
        },
        less = {
            validate = true,
            lint = {
                unknownAtRules = "ignore",
            },
        },
        capabilities = capabilities,
    }

    --------------------- vim-lsp Config --------------------------------------
    require("lspconfig").vimls.setup {
        on_attach = on_attach,
        capabilities = capabilities,
    }

    --------------------- toml-lsp Config --------------------------------------
    require("lspconfig").taplo.setup {
        on_attach = on_attach,
        capabilities = capabilities,
    }

    --------------------- go-lsp Config --------------------------------------
    require("lspconfig").gopls.setup {
        on_attach = on_attach,
        gopls = {
            completeUnimported = true,
            usePlaceholders = true,
            gofumpt = true,
            staticcheck = true,
            analyses = {
                unusedparams = true,
            },
            hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                compositeLiteralTypes = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
            },
        },
        capabilities = capabilities,
    }
    ---
    ---
    ---
    ---
    ---
    ---
    -- setup null-ls
    require("mason-null-ls").setup { ensure_installed = ensure_installed }

    local none_ls = require "null-ls"
    local formatting = none_ls.builtins.formatting
    local diagnostics = none_ls.builtins.diagnostics
    local code_actions = none_ls.builtins.code_actions
    none_ls.setup {
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
            -- if client.supports_method("textDocument/formatting") then
            -- 	local format_group = vim.api.nvim_create_augroup("autoformat", { clear = true })
            -- 	vim.api.nvim_create_autocmd("BufWritePre", {
            -- 		group = format_group,
            -- 		buffer = bufnr,
            -- 		callback = function()
            -- 			vim.cmd("NullFormat")
            -- 		end,
            -- 	})
            -- end
        end,
        sources = {
            --formatting
            formatting.djlint,
            formatting.markdownlint,
            formatting.beautysh,
            formatting.stylua,
            -- formatting.black.with({ extra_args = { "--line-length 100" } }),
            -- formatting.isort,
            none_ls.builtins.formatting.prettier.with {
                extra_filetypes = { "toml" },
                extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" },
            },

            --diagnostics
            -- diagnostics.eslint_d,
            diagnostics.markdownlint,
            diagnostics.djlint,
            -- diagnostics.flake8.with({ extra_args = { "--max-line-length", "100" } }),
            diagnostics.shellcheck,
            diagnostics.jsonlint,

            -- code_actions
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
