local astro_plugins = {

	["https://git.sr.ht/~whynothugo/lsp_lines.nvim"] = {
		commit = "dbfd8e96ec2696e1ceedcd23fd70e842256e3dea",
		config = function()
			require("lsp_lines").setup()
		end,
	},
	["braxtons12/blame_line.nvim"] = {
		commit = "f77bd2d90b51c10eb70b36471f2207ae516fa84a",
		config = function()
			require("blame_line").disable()
		end,
	},
	["bennypowers/nvim-regexplainer"] = {
		commit = "0d7151ddd3ff2b2e9e8a69137b911c28fc7051a4",
		config = function()
			require("regexplainer").setup({
				auto = true,
				filetypes = {
					"html",
					"js",
					"cjs",
					"py",
					"mjs",
					"ts",
					"jsx",
					"tsx",
					"cjsx",
					"mjsx",
				},
				display = "popup",

				mappings = {
					toggle = "gR",
					-- examples, not defaults:
					-- show = 'gS',
					-- hide = 'gH',
					-- show_split = 'gP',
					-- show_popup = 'gU',
				},
			})
		end,
		requires = {
			"nvim-treesitter/nvim-treesitter",
			"MunifTanjim/nui.nvim",
		},
	},
	--vim be good
	["ThePrimeagen/vim-be-good"] = {},
	["kylechui/nvim-surround"] = {
		commit = "81f672ad6525b5d8cc27bc6ff84636cc12664485",
		config = function()
			require("nvim-surround").setup({
				-- Configuration here, or leave empty to use defaults
			})
		end,
	},

	["mg979/vim-visual-multi"] = {
		commit = "724bd53adfbaf32e129b001658b45d4c5c29ca1a",
	},

	["ellisonleao/gruvbox.nvim"] = {
		commit = "24f9e795bfac5fabbaba703116e747dcf2ad8d2f",
	},

	["Issafalcon/lsp-overloads.nvim"] = {
		commit = "ad00f4e4e00cd8b2dd355587a9428330ca40de2b",
	},

	["nvim-treesitter/nvim-treesitter-context"] = {
		commit = "2466e8007290b6228183171965b6ce1a2f111995",
	},

	["folke/trouble.nvim"] = {
		commit = "ed65f84abc4a1e5d8f368d7e02601fc0357ea15e",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("trouble").setup({
				-- your configuration comes here
				-- or leave it empty to use the default settings
				vim.api.nvim_set_keymap("n", "<leader>xx", "<cmd>TroubleToggle<cr>", { silent = true, noremap = true }),
				vim.api.nvim_set_keymap(
					"n",
					"<leader>xw",
					"<cmd>Trouble workspace_diagnostics<cr>",
					{ silent = true, noremap = true }
				),
				vim.api.nvim_set_keymap(
					"n",
					"<leader>xd",
					"<cmd>Trouble document_diagnostics<cr>",
					{ silent = true, noremap = true }
				),
				vim.api.nvim_set_keymap(
					"n",
					"<leader>xl",
					"<cmd>Trouble loclist<cr>",
					{ silent = true, noremap = true }
				),
				vim.api.nvim_set_keymap(
					"n",
					"<leader>xq",
					"<cmd>Trouble quickfix<cr>",
					{ silent = true, noremap = true }
				),
				vim.api.nvim_set_keymap(
					"n",
					"gR",
					"<cmd>Trouble lsp_references<cr>",
					{ silent = true, noremap = true }
				),
				-- refer to the configuration section below
			})
		end,
	},
	-- Plugin manager
	["wbthomason/packer.nvim"] = {},

	["folke/tokyonight.nvim"] = {
		commit = "9fba0cdd05382a427dafaa2b8ebb4aba99126bc0",
		config = function()
			require("tokyonight").setup({
				style = "night", -- The theme comes in three styles, `storm`, a darker variant `night` and `day`
				transparent = false, -- Enable this to disable setting the background color
				terminal_colors = true, -- Configure the colors used when opening a `:terminal` in Neovim                                                                                                  |
				styles = {
					comments = "italic",
					keywords = "italic",
					functions = "bold",
					sidebars = "dark", -- style for sidebars, see below
					floats = "dark", -- style for floating windows
				},
				sidebars = { "qf", "help", "Neotree" }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`                                                      |
				lualine_bold = true, -- When `true`, section headers in the lualine theme will be bold                                                                                                  |
			})
		end,
	},
	-- Optimiser
	["lewis6991/impatient.nvim"] = {},

	-- Lua functions
	["nvim-lua/plenary.nvim"] = { module = "plenary" },

	-- Popup API
	["nvim-lua/popup.nvim"] = {},

	-- Indent detection
	["Darazaki/indent-o-matic"] = {
		event = "BufReadPost",
		config = function()
			require("configs.indent-o-matic")
		end,
	},

	-- Notification Enhancer
	["rcarriga/nvim-notify"] = {
		event = "VimEnter",
		config = function()
			require("configs.notify")
		end,
	},

	-- Neovim UI Enhancer
	["stevearc/dressing.nvim"] = {
		event = "VimEnter",
		config = function()
			require("configs.dressing")
		end,
	},

	-- Cursorhold fix
	["antoinemadec/FixCursorHold.nvim"] = {
		event = { "BufRead", "BufNewFile" },
		config = function()
			vim.g.cursorhold_updatetime = 100
		end,
	},

	-- Smarter Splits
	["mrjones2014/smart-splits.nvim"] = {
		module = "smart-splits",
		config = function()
			require("configs.smart-splits")
		end,
	},

	-- Icons
	["kyazdani42/nvim-web-devicons"] = {
		event = "VimEnter",
		config = function()
			require("configs.icons")
		end,
	},

	-- LSP Icons
	["onsails/lspkind.nvim"] = {
		module = "lspkind",
		config = function()
			require("configs.lspkind")
		end,
	},

	-- Bufferline
	["akinsho/bufferline.nvim"] = {
		after = "nvim-web-devicons",
		config = function()
			require("configs.bufferline")
		end,
	},

	-- Better buffer closing
	["famiu/bufdelete.nvim"] = { commit = "46255e4a76c4fb450a94885527f5e58a7d96983c", cmd = { "Bdelete", "Bwipeout" } },

	["s1n7ax/nvim-window-picker"] = {
		tag = "v1.*",
		module = "window-picker",
		config = function()
			require("configs.window-picker")
		end,
	},

	-- File explorer
	["nvim-neo-tree/neo-tree.nvim"] = {
		branch = "v2.x",
		module = "neo-tree",
		cmd = "Neotree",
		requires = { { "MunifTanjim/nui.nvim", module = "nui" } },
		setup = function()
			vim.g.neo_tree_remove_legacy_commands = true
		end,
		config = function()
			require("configs.neo-tree")
		end,
	},

	-- Statusline
	["nvim-lualine/lualine.nvim"] = {
		after = "nvim-web-devicons",
		config = function()
			require("configs.lualine")
		end,
	},

	-- Parenthesis highlighting
	["p00f/nvim-ts-rainbow"] = { after = "nvim-treesitter" },

	-- Autoclose tags
	["windwp/nvim-ts-autotag"] = { after = "nvim-treesitter" },

	-- Context based commenting
	["JoosepAlviste/nvim-ts-context-commentstring"] = { after = "nvim-treesitter" },

	-- Syntax highlighting
	["nvim-treesitter/nvim-treesitter"] = {
		run = ":TSUpdate",
		event = { "BufRead", "BufNewFile" },
		cmd = {
			"TSInstall",
			"TSInstallInfo",
			"TSInstallSync",
			"TSUninstall",
			"TSUpdate",
			"TSUpdateSync",
			"TSDisableAll",
			"TSEnableAll",
		},
		config = function()
			require("configs.treesitter")
		end,
	},

	-- Snippet collection
	["rafamadriz/friendly-snippets"] = { opt = true },

	-- Snippet engine
	["L3MON4D3/LuaSnip"] = {
		module = "luasnip",
		wants = "friendly-snippets",
		config = function()
			require("configs.luasnip")
		end,
	},

	-- Completion engine
	["hrsh7th/nvim-cmp"] = {
		event = "InsertEnter",
		config = function()
			require("configs.cmp")
		end,
	},

	-- Snippet completion source
	["saadparwaiz1/cmp_luasnip"] = {
		after = "nvim-cmp",
		config = function()
			astronvim.add_user_cmp_source("luasnip")
		end,
	},

	-- Buffer completion source
	["hrsh7th/cmp-buffer"] = {
		after = "nvim-cmp",
		config = function()
			astronvim.add_user_cmp_source("buffer")
		end,
	},

	-- Path completion source
	["hrsh7th/cmp-path"] = {
		after = "nvim-cmp",
		config = function()
			astronvim.add_user_cmp_source("path")
		end,
	},

	-- LSP completion source
	["hrsh7th/cmp-nvim-lsp"] = {
		after = "nvim-cmp",
		config = function()
			astronvim.add_user_cmp_source("nvim_lsp")
		end,
	},

	-- Package Manager
	["williamboman/mason.nvim"] = {
		config = function()
			require("configs.mason")
		end,
	},

	["WhoIsSethDaniel/mason-tool-installer.nvim"] = {
		after = "mason.nvim",
		config = function()
			require("configs.mason-tool-installer")
		end,
	},

	-- Built-in LSP
	["neovim/nvim-lspconfig"] = {},

	-- LSP manager
	["williamboman/mason-lspconfig.nvim"] = {
		after = { "mason.nvim", "nvim-lspconfig" },
		config = function()
			require("configs.lsp")
		end,
	},

	-- LSP symbols
	["stevearc/aerial.nvim"] = {
		commit = "b9f6067529ef123b8ace705ea356869f66aad320",
		module = "aerial",
		cmd = { "AerialToggle", "AerialOpen", "AerialInfo" },
		config = function()
			require("configs.aerial")
		end,
	},

	-- Formatting and linting
	["jose-elias-alvarez/null-ls.nvim"] = {
		event = { "BufRead", "BufNewFile" },
		config = function()
			require("configs.null-ls")
		end,
	},

	-- Fuzzy finder
	["nvim-telescope/telescope.nvim"] = {
		cmd = "Telescope",
		module = "telescope",
		config = function()
			require("configs.telescope")
		end,
	},

	-- Fuzzy finder syntax support
	[("nvim-telescope/telescope-%s-native.nvim"):format(vim.fn.has("win32") == 1 and "fzy" or "fzf")] = {
		after = "telescope.nvim",
		run = vim.fn.has("win32") ~= 1 and "make" or nil,
		config = function()
			require("telescope").load_extension(vim.fn.has("win32") == 1 and "fzy_native" or "fzf")
		end,
	},

	-- Git integration
	["lewis6991/gitsigns.nvim"] = {
		event = "BufEnter",
		config = function()
			require("configs.gitsigns")
		end,
	},

	-- Start screen
	["goolord/alpha-nvim"] = {
		cmd = "Alpha",
		module = "alpha",
		config = function()
			require("configs.alpha")
		end,
	},

	-- Color highlighting
	["NvChad/nvim-colorizer.lua"] = {
		event = { "BufRead", "BufNewFile" },
		config = function()
			require("configs.colorizer")
		end,
	},

	-- Autopairs
	["windwp/nvim-autopairs"] = {
		event = "InsertEnter",
		config = function()
			require("configs.autopairs")
		end,
	},

	-- Terminal
	["akinsho/toggleterm.nvim"] = {
		cmd = "ToggleTerm",
		module = { "toggleterm", "toggleterm.terminal" },
		config = function()
			require("configs.toggleterm")
		end,
	},

	-- Commenting
	["numToStr/Comment.nvim"] = {
		module = { "Comment", "Comment.api" },
		keys = { "gc", "gb", "g<", "g>" },
		config = function()
			require("configs.Comment")
		end,
	},

	-- Indentation
	["lukas-reineke/indent-blankline.nvim"] = {
		event = "BufRead",
		config = function()
			require("configs.indent-line")
		end,
	},

	-- Keymaps popup
	["folke/which-key.nvim"] = {
		module = "which-key",
		config = function()
			require("configs.which-key")
		end,
	},

	-- Smooth scrolling
	["declancm/cinnamon.nvim"] = {
		event = { "BufRead", "BufNewFile" },
		config = function()
			require("configs.cinnamon")
		end,
	},

	-- Smooth escaping
	["max397574/better-escape.nvim"] = {
		event = "InsertCharPre",
		config = function()
			require("configs.better_escape")
		end,
	},

	-- Get extra JSON schemas
	["b0o/SchemaStore.nvim"] = { module = "schemastore" },

	-- Session manager
	["Shatur/neovim-session-manager"] = {
		module = "session_manager",
		cmd = "SessionManager",
		event = "BufWritePost",
		config = function()
			require("configs.session_manager")
		end,
	},
}

if astronvim.updater.snapshot then
	for plugin, options in pairs(astro_plugins) do
		local pin = astronvim.updater.snapshot[plugin:match("/([^/]*)$")]
		options.commit = pin and pin.commit or options.commit
	end
end

local user_plugin_opts = astronvim.user_plugin_opts
local status_ok, packer = pcall(require, "packer")
if status_ok then
	packer.startup({
		function(use)
			for key, plugin in pairs(user_plugin_opts("plugins.init", astro_plugins)) do
				if type(key) == "string" and not plugin[1] then
					plugin[1] = key
				end
				use(plugin)
			end
		end,
		config = user_plugin_opts("plugins.packer", {
			compile_path = astronvim.default_compile_path,
			display = {
				open_fn = function()
					return require("packer.util").float({ border = "rounded" })
				end,
			},
			profile = {
				enable = true,
				threshold = 0.0001,
			},
			git = {
				clone_timeout = 300,
				subcommands = {
					update = "pull --rebase",
				},
			},
			auto_clean = true,
			compile_on_sync = true,
		}),
	})
end