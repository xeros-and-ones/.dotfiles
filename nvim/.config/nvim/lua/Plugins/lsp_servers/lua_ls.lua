return {
    before_init = require("neodev.lsp").before_init,
    on_init = function(client)
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentFormattingRangeProvider = false
    end,
    settings = {
        Lua = {
            semantic = {
                enable = false,
            },
            hint = { enable = true },
            diagnostics = {
                globals = { "vim" },
            },
            telemetry = { enable = false },
            workspace = {
                library = {
                    [vim.fn.expand "$VIMRUNTIME/lua"] = true,
                    [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
                    [vim.fn.stdpath "data" .. "/lazy/lazy.nvim/lua/lazy"] = true,
                },
                maxPreload = 10000,
                preloadFileSize = 1000,
            },
        },
    },
}
