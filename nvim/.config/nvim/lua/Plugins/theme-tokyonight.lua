local M = {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
}

function M.config()
    require("tokyonight").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        style = "night",         -- The theme comes in three styles, `storm`, `moon`, a darker variant `night` and `day`
        light_style = "day",     -- The theme is used when the background is set to light
        transparent = true,      -- Enable this to disable setting the background color
        terminal_colors = false, -- Configure the colors used when opening a `:terminal` in [Neovim](https://github.com/neovim/neovim)
        styles = {
            -- Style to be applied to different syntax groups
            -- Value is any valid attr-list value for `:help nvim_set_hl`
            comments = { italic = true },
            keywords = { italic = true },
            functions = {},
            variables = {},
            -- Background styles. Can be "dark", "transparent" or "normal"
            sidebars = "transparent",            -- style for sidebars, see below
            floats = "transparent",              -- style for floating windows
        },
        sidebars = { "qf", "help", "neo-tree" }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`
        day_brightness = 0.3,                    -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
        hide_inactive_statusline = false,        -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
        dim_inactive = false,                    -- dims inactive windows
        lualine_bold = false,                    -- When `true`, section headers in the lualine theme will be bold

        --- You can override specific color groups to use other groups or a hex color
        --- function will be called with a ColorScheme table
        -- ---@param colors ColorScheme
        -- on_colors = function(colors) end,

        --- You can override specific highlights to use other groups or a hex color
        --- function will be called with a Highlights and ColorScheme table
        on_highlights = function(hl, c)
            hl.PMenu = { bg = "NONE" }
            hl.WhichKey = { link = "Mormal" }
            -- SignColumn = { bg = "none" }
            hl.MsgArea = { bg = "none" }
            hl.TreesitterContext = { bg = "darkslategray" }
            hl.TreesitterContextLineNumber = { bg = "darkslategray" }
            hl.Todo = { link = "Comment" }
            -- standard colours for debug sign icons
            hl.DapBreakpoint = { fg = "#993939" }
            hl.DapLogPoint = { fg = "#61afef" }
            hl.DapStopped = { fg = "#98c379" }
            -- link neotree colours to nvim-tree for automatic theme support
            hl.NeoTreeDirectoryIcon = { link = "NvimTreeFolderIcon" }
            hl.NeoTreeDirectoryName = { link = "NvimTreeOpenedFolderName" }
            hl.NeoTreeSymbolicLinkTarget = { link = "NvimTreeSymlink" }
            hl.NeoTreeRootName = { link = "NvimTreeRootFolder" }
            hl.NeoTreeFileNameOpened = { link = "NvimTreeOpenedFile" }
            -- linked groups for all themes
            hl.TroubleCount = { link = "DiagnosticOk" }
            hl.TroubleTextHint = { link = "DiagnosticHint" }
            hl.TroubleTextError = { link = "DiagnosticError" }
            hl.TroubleTextWarning = { link = "DiagnosticWarn" }
            hl.TroubleTextInformation = { link = "DiagnosticInfo" }
        end,
    })
end

return M
