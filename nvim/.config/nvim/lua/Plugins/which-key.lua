local M = {
    "folke/which-key.nvim",
    commit = "7ccf476ebe0445a741b64e36c78a682c1c6118b7",
    enabled = true,
    event = "VeryLazy",
}

M.init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
end

function M.config()
    local wk = require "which-key"
    wk.setup {
        plugins = {
            marks = true,     -- shows a list of your marks on ' and `
            registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
            -- the presets plugin, adds help for a bunch of default keybindings in Neovim
            spelling = {
                enabled = true,   -- enabling this will show WhichKey when pressing z= to select spelling suggestions
                suggestions = 20, -- how many suggestions should be shown in the list?
            },
            presets = {
                operators = true,    -- adds help for operators like d, y, ...
                motions = true,      -- adds help for motions
                text_objects = true, -- help for text objects triggered after entering an operator
                windows = true,      -- default bindings on <c-w>
                nav = true,          -- misc bindings to work with windows
                z = true,            -- bindings for folds, spelling and others prefixed with z
                g = true,            -- bindings for prefixed with g
            },
        },
        key_labels = {
            -- override the label used to display some keys. It doesn't effect WK in any other way.
            -- For example:
            ["<leader>"] = "LDR",
            ["<cr>"] = "RET",
            ["<tab>"] = "TAB",
        },
        defaults = {
            color_devicons = true,
        },
        motions = {
            count = true,
        },
        show_help = false,
        window = {
            border = "rounded",
            padding = { 1, 0, 1, 0 },
            winblend = 00,
            zindex = 1000,
        },
        layout = {
            height = { min = 4, max = 25 }, -- min and max height of the columns
            width = { min = 20, max = 50 }, -- min and max width of the columns
            spacing = 3,                    -- spacing between columns
            align = "left",                 -- align columns left, center or right
        },
        icons = {
            breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
            separator = "➜", -- symbol used between a key and it's label
            group = "+", -- symbol prepended to a group
        },
    }
    wk.register({
        l = { name = "LSP" },
    }, { prefix = "<leader>" })

    wk.register({
        b = { name = "Buffers" },
    }, { prefix = "<leader>" })

    wk.register({
        f = { name = "Find" },
    }, { prefix = "<leader>" })

    wk.register({
        g = { name = "Git Control" },
    }, { prefix = "<leader>" })

    wk.register({
        D = { name = "Dadbod Database" },
    }, { prefix = "<leader>" })


    wk.register({
        s = { name = "Spectre Find & Replace" },
    }, { prefix = "<leader>" })

    wk.register({
        z = { name = "Utilities" },
    }, { prefix = "<leader>" })
    wk.register(
        {
            c = { name = "Code", t = { name = "Neotest" } }
        },
        { prefix = "<leader>" }
    )
end

return M
