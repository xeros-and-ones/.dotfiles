local M = {
    "nvim-treesitter/nvim-treesitter",
    enabled = true,
    cmd = {
        "TSInstall",
        "TSUninstall",
        "TSInstallInfo",
        "TSUpdate",
        "TSBufEnable",
        "TSBufDisable",
        "TSEnable",
        "TSDisable",
        "TSModuleInfo",
        "TSToggle",
        "TSBufToggle",
    },
    build = function()
        require("nvim-treesitter.install").update { with_sync = true }
    end,
    dependencies = {
        "nvim-treesitter/nvim-treesitter-textobjects",
    },
}

function M.config()
    require("nvim-treesitter.install").compilers = { "gcc", "clang", "mingw" }
    require("nvim-treesitter.configs").setup {
        ensure_installed = {
            "bash",
            "c",
            "c_sharp",
            "comment",
            "cpp",
            "css",
            "dockerfile",
            "gitignore",
            "go",
            "html",
            "htmldjango",
            "javascript",
            "jsdoc",
            "json",
            "json5",
            "jsonc",
            "lua",
            "markdown",
            "markdown_inline",
            "python",
            "query",
            "regex",
            "rst",
            "rust",
            "sql",
            "toml",
            "tsx",
            "typescript",
            "vim",
            "vimdoc",
            "yaml",
            "java",
        },
        auto_install = true, -- disable if no tree-sitter cli installed
        ignore_install = {}, -- list of parsers to ignore installing
        highlight = {
            enable = true,
            additional_vim_regex_highlighting = true,
        },
        indent = { enabled = true },
        incremental_selection = {
            enable = true,
            keymaps = {
                init_selection = "vv",
                node_incremental = "+",
                scope_incremental = false,
                node_decremental = "_",
            },
        },
        context_commentstring = {
            enable = true,
            enable_autocmd = false,
        },
        textobjects = {
            swap = {
                enable = false,
                swap_next = {
                    ["<leader>a"] = "@parameter.inner",
                },
                swap_previous = {
                    ["<leader>A"] = "@parameter.inner",
                },
            },
            select = {
                enable = true,
                lookahead = true,

                keymaps = {
                    -- You can use the capture groups defined in textobjects.scm
                    ["af"] = { query = "@function.outer", desc = "around a function" },
                    ["if"] = { query = "@function.inner", desc = "inner part of a function" },
                    ["ac"] = { query = "@class.outer", desc = "around a class" },
                    ["ic"] = { query = "@class.inner", desc = "inner part of a class" },
                    ["ai"] = { query = "@conditional.outer", desc = "around an if statement" },
                    ["ii"] = { query = "@conditional.inner", desc = "inner part of an if statement" },
                    ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
                },
                selection_modes = {
                    ["@parameter.outer"] = "v", -- charwise
                    ["@parameter.inner"] = "v", -- charwise
                    ["@function.outer"] = "v", -- charwise
                    ["@conditional.outer"] = "V", -- linewise
                    ["@loop.outer"] = "V", -- linewise
                    ["@class.outer"] = "<c-v>", -- blockwise
                },
                include_surrounding_whitespace = true,
            },
            move = {
                enable = true,
                set_jumps = true, -- whether to set jumps in the jumplist
                goto_previous_start = {
                    ["[f"] = { query = "@function.outer", desc = "Previous function" },
                    ["[c"] = { query = "@class.outer", desc = "Previous class" },
                    ["[s"] = { query = "@scope", query_group = "locals", desc = "Previous scope" },
                    ["[z"] = { query = "@fold", query_group = "folds", desc = "Previous fold" },
                },
                goto_next_start = {
                    ["]f"] = { query = "@function.outer", desc = "Next function" },
                    ["]c"] = { query = "@class.outer", desc = "Next class" },
                    ["]s"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
                    ["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
                },
            },
        },
    }
end

return M
