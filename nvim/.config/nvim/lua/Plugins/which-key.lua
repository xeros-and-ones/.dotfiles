local M = {
    "folke/which-key.nvim",
    enabled = true,
    event = "VeryLazy",
}

function M.config()
    local wk = require "which-key"
    wk.setup {
        plugins = {
            marks = true, -- shows a list of your marks on ' and `
            registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
            -- the presets plugin, adds help for a bunch of default keybindings in Neovim
            spelling = {
                enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
                suggestions = 20, -- how many suggestions should be shown in the list?
            },
            presets = {
                operators = true, -- adds help for operators like d, y, ...
                motions = true, -- adds help for motions
                text_objects = true, -- help for text objects triggered after entering an operator
                windows = true, -- default bindings on <c-w>
                nav = true, -- misc bindings to work with windows
                z = true, -- bindings for folds, spelling and others prefixed with z
                g = true, -- bindings for prefixed with g
            },
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
            padding = { 0, 0, 0, 0 },
            winblend = 00,
            zindex = 1000,
        },
        layout = {
            height = { min = 4, max = 25 }, -- min and max height of the columns
            width = { min = 5, max = 40 }, -- min and max width of the columns
            spacing = 1, -- spacing between columns
            align = "left", -- align columns left, center or right
        },
        icons = {
            breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
            separator = "➜", -- symbol used between a key and it's label
            group = "_", -- symbol prepended to a group
        },
    }

    wk.register({
        t = { name = "_Testing" },
        l = { name = "_LSP" },
        z = { name = "_Utilities" },
        s = { name = "_Spectre Find & Replace" },
        D = { name = "_Dadbod Database" },
        g = { name = "_Git Control" },
        f = { name = "_Find" },
        b = { name = "_Buffers" },
    }, { prefix = "<leader>" })
end

return M
