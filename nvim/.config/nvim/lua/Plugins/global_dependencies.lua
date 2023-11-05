return {
    "nvim-lua/plenary.nvim",
    "b0o/schemastore.nvim",
    "microsoft/vscode-codicons",
    {
        "nvim-tree/nvim-web-devicons",
        event = "VeryLazy",
        config = function(_, opts)
            require("nvim-web-devicons").setup {
                override = {
                    zsh = {
                        icon = "",
                        color = "#428850",
                        cterm_color = "65",
                        name = "Zsh",
                    },
                },
                color_icons = true,
                default = true,
            }
        end,
    },
    {
        "danymat/neogen",
        enabled = true,
        event = { "BufRead", "BufNewFile" },
        opts = {
            snippet_engine = "luasnip",
            languages = {
                python = {
                    template = {
                        annotation_convention = "google_docstrings",
                    },
                },
            },
        },
    },
    {
        "moll/vim-bbye",
        cmd = { "Bdelete", "Bwipeout" },
    },
    {
        "stevearc/dressing.nvim",
        commit = "8f4d62b7817455896a3c73cab642002072c114bc",
        enabled = true,
        event = "VeryLazy",
        opts = {
            input = {
                enabled = true,
                default_prompt = "➤ ",
                win_options = {
                    winblend = 0,
                },
            },
            select = {
                enabled = true,
                backend = { "telescope", "builtin" },
                builtin = {
                    win_options = {
                        winblend = 0,
                    },
                },
            },
        },
    },
}
