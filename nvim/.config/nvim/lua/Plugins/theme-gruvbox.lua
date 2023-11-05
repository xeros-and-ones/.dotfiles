local M = {
    "ellisonleao/gruvbox.nvim",
    commit = "a569160337aa309db4cfe21ba4e088f0e6676fb4",
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
}

M.opts = {
    undercurl = true,
    bold = true,
    italic = {
        strings = true,
        comments = true,
        operators = false,
        folds = true,
    },
    stikethrough = true,
    invert_selection = false,
    invert_signs = false,
    invert_tabline = false,
    invert_indent_guides = false,
    inverse = true,
    contrast = "hard",
    transparent_mode = true,
    overrides = {
        PMenu = { bg = "NONE" },
        DiagnosticVirtualTextError = { fg = "#fb4934", bg = "#400404" },
        DiagnosticVirtualTextWarn = { fg = "#fabd2f", bg = "#3f4004" },
        DiagnosticVirtualTextInfo = { fg = "#83a598", bg = "#040540" },
        DiagnosticVirtualTextHint = { fg = "#427b58", bg = "#043d40" },
        WhichKey = { link = "Mormal" },
        -- SignColumn = { bg = "none" },
        MsgArea = { bg = "none" },
        TreesitterContext = { bg = "darkslategray" },
        TreesitterContextLineNumber = { bg = "darkslategray" },
        Todo = { link = "Comment" },
        -- standard colours for debug sign icons
        DapBreakpoint = { fg = "#993939" },
        DapLogPoint = { fg = "#61afef" },
        DapStopped = { fg = "#98c379" },
        -- link neotree colours to nvim-tree for automatic theme support
        NeoTreeDirectoryIcon = { link = "NvimTreeFolderIcon" },
        NeoTreeDirectoryName = { link = "NvimTreeOpenedFolderName" },
        NeoTreeSymbolicLinkTarget = { link = "NvimTreeSymlink" },
        NeoTreeRootName = { link = "NvimTreeRootFolder" },
        NeoTreeFileNameOpened = { link = "NvimTreeOpenedFile" },
        -- linked groups for all themes
        TroubleCount = { link = "DiagnosticOk" },
        TroubleTextHint = { link = "DiagnosticHint" },
        TroubleTextError = { link = "DiagnosticError" },
        TroubleTextWarning = { link = "DiagnosticWarn" },
        TroubleTextInformation = { link = "DiagnosticInfo" },
        FlashPrompt = { link = "DiagnosticVirtualTextHint" },
    },
}

return M
