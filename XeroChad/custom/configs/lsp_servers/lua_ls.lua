return {
	-- before_init = require("neodev.lsp").before_init,
	settings = {
		Lua = {
			semantic = {
				enable = true,
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
				maxPreload = 10000,
				preloadFileSize = 1000,
			},
		},
	},
}
