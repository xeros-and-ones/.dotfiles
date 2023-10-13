local M = {
    "L3MON4D3/LuaSnip",
    -- follow latest release.
    version = "v2.*",
    build = "make install_jsregexp",
    dependencies = {
        "rafamadriz/friendly-snippets",
        config = function()
        end,
    },
}
M.opts = {
    history = true,
    update_events = "TextChanged,TextChangedI",
    delete_check_events = "TextChanged,InsertLeave",
    enable_autosnippets = true,
}
function M.config()
    require("luasnip.loaders.from_vscode").lazy_load()
    require 'luasnip'.filetype_extend("django", { "django", "djangohtml" })
    require 'luasnip'.filetype_extend("djangohtml", { "djangohtml" })
    require 'luasnip'.filetype_extend("python", { "pydoc" })
end

return M
