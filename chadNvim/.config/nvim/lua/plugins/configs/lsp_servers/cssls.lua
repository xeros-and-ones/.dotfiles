return {
    on_init = function(client)
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentFormattingRangeProvider = false
    end,
    settings = {
        css = {
            validate = true,
            lint = {
                unknownAtRules = "ignore",
            },
        },
        scss = {
            validate = true,
            lint = {
                unknownAtRules = "ignore",
            },
        },
        less = {
            validate = true,
            lint = {
                unknownAtRules = "ignore",
            },
        },
    },
}
