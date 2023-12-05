return {
	settings = {
		Lua = {
			semantic = {
				enable = true,
			},
			hint = {
				enable = true,
			},
			diagnostics = {
				globals = { "vim" },
			},
			telemetry = { enable = false },
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
					[vim.fn.stdpath("data") .. "/lazy/lazy.nvim/lua/lazy"] = true,
					[vim.fn.stdpath("data") .. "/lazy/ui/nvchad_types"] = true,
				},
				checkThirdParty = false,
				maxPreload = 100000,
				preloadFileSize = 10000,
			},
		},
	},
}
