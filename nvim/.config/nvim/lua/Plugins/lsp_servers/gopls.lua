return {
    on_init = function(client)
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentFormattingRangeProvider = false
    end,
    settings = {
        gopls = {
            completeUnimported = true,
            usePlaceholders = true,
            gofumpt = true,
            staticcheck = true,
            analyses = {
                unusedparams = true,
            },
            hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                compositeLiteralTypes = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
            },
        },
    },
}
