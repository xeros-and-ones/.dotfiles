local M = {
  "bennypowers/nvim-regexplainer",
  commit = "4250c8f3c1307876384e70eeedde5149249e154f",
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
      "cjsx",
      "mjsx",
    },

    -- Whether to log debug messages
    debug = false,

    -- 'split', 'popup'
    display = "popup",

    mappings = {
      toggle = "gR",
      -- examples, not defaults:
      -- show = 'gS',
      -- hide = 'gH',
      -- show_split = 'gP',
      show_popup = "gU",
    },

    narrative = {
      separator = "\n",
    },
  }
end

return M
