local M = {
    "bennypowers/nvim-regexplainer",
    commit = "4250c8f3c1307876384e70eeedde5149249e154f",
    cmd = { "RegexplainerShowSplit", "RegexplainerShowPopup", "RegexplainerHide", "RegexplainerToggle" },
}

M.config = function()
    -- defaults
    require("regexplainer").setup {
        -- 'narrative'
        mode = "narrative", -- TODO: 'ascii', 'graphical'

        -- automatically show the explainer when the cursor enters a regexp
        auto = false,

        -- filetypes (i.e. extensions) in which to run the autocommand
        filetypes = {
            "html",
            "js",
            "cjs",
            "mjs",
            "ts",
            "jsx",
            "tsx",
            "py",
            "cjsx",
            "mjsx",
        },

        -- Whether to log debug messages
        debug = false,

        -- 'split', 'popup'
        display = "popup",

        popup = {
            border = {
                padding = { 1, 1 },
                style = "rounded"
            }
        },

        narrative = {
            separator = function(component)
                local sep = '\n';
                if component.depth > 0 then
                    for _ = 1, component.depth do
                        sep = sep .. '> '
                    end
                end
                return sep
            end
        },
    }
end

return M
