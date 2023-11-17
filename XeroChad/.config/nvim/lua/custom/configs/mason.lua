return {
	ui = {
		check_outdated_packages_on_open = true,
		border = "rounded",
		icons = {
			package_pending = " ",
			package_installed = "󰄳 ",
			package_uninstalled = " ",
		},

		keymaps = {
			toggle_server_expand = "<CR>",
			install_server = "i",
			update_server = "u",
			check_server_version = "c",
			update_all_servers = "U",
			check_outdated_servers = "C",
			uninstall_server = "X",
			cancel_installation = "<C-c>",
		},
	},
	registries = {
		"github:mason-org/mason-registry",
		"lua:custom.configs.lsp_servers.custom",
	},

	max_concurrent_installers = 3,
	ensure_installed = {
		-- lsp ---------------------------------------
		"lua-language-server",
		"taplo",
		"vim-language-server",
		"css-lsp",
		"html-lsp",
		"python-lsp-server",
		"typescript-language-server",
		"yaml-language-server",
		"json-lsp",
		"gopls",
		"sqlls",
		"clangd",
		"asm-lsp",

		-- debuggers ---------------------------------
		"codelldb",
		"cpptools",
		"debugpy",
		"go-debug-adapter",
		"firefox-debug-adapter",
		"chrome-debug-adapter",
		"js-debug-adapter",
		"node-debug2-adapter",
		"dart-debug-adapte",
		"netcoredbg",
		-- linters -----------------------------------
		"markdownlint",
		"djlint",
		"shellcheck",
		"jsonlint",

		-- formatters -------------------------------
		"beautysh",
		"stylua",
		"gofumpt",
		"clang-format",
		"prettier",
		"black",
		"cmakelang",
		"isort",
		"asmfmt",
	},
}
