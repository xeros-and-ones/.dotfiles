return {
	-- before_init = require("neodev.lsp").before_init,
	single_file_support = true,
	on_init = function(client)
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentFormattingRangeProvider = false
	end,
	settings = {
		Lua = {
			semantic = {
				enable = true,
			},
			runtime = {
				version = "LuaJIT",
				special = {
					reload = "require",
				},
			},
			hint = { enable = true },
			diagnostics = {
				globals = { "vim" },
			},
			telemetry = { enable = false },
			completion = {
				callSnippet = "Replace",
			},
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
					[vim.fn.stdpath("data") .. "/lazy/lazy.nvim/lua/lazy"] = true,
					[vim.fn.stdpath("data") .. "/lazy/ui/nvchad_types"] = true,
				},
				checkThirdParty = false,
				maxPreload = 10000,
				preloadFileSize = 1000,
			},
		},
	},
}
