return {
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
