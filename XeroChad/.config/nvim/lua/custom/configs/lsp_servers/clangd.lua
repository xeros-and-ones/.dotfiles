return {
	on_init = function(client)
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentFormattingRangeProvider = false
	end,
	single_file_support = true,
	cmd = {
		"clangd",
		-- "--all-scopes-completion",
		-- "--suggest-missing-includes",
		-- "--background-index",
		-- "--pch-storage=disk",
		-- "--cross-file-rename",
		-- "--log=info",
		-- "--completion-style=detailed",
		-- "--enable-config", -- clangd 11+ supports reading from .clangd configuration file
		-- "--clang-tidy",
		"--offset-encoding=utf-16",
		-- "--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type",
		-- "--fallback-style=Google",
		-- "--header-insertion=never",
		-- "--query-driver=<list-of-white-listed-complers>"
	},
}
