return {
	on_init = function(client)
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentFormattingRangeProvider = false
	end,
	single_file_support = true,
	cmd = {
		"clangd",
		"--all-scopes-completion",
		"--suggest-missing-includes",
		"--inlay-hints=true",
		"--enable-config",
		"--completion-style=detailed",
	},
}
