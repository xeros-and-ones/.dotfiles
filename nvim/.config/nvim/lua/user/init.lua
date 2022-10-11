--              AstroNvim Configuration Table
-- All configuration changes should go inside of the table below

-- You can think of a Lua "table" as a dictionary like data structure the
-- normal format is "key = value". These also handle array like data structures
-- where a value with no key simply has an implicit numeric key
local config = {

	-- Set colorscheme to use
	colorscheme = "gruvbox",

	-- set vim options here (vim.<first_key>.<second_key> =  value)
	-- options = {
	-- 	opt = {
	-- 		relativenumber = true, -- sets vim.opt.relativenumber
	-- 	},
	-- 	g = {
	-- 		mapleader = " ", -- sets vim.g.mapleader
	-- 	},
	-- },
	-- If you need more control, you can use the function()...end notation
	options = function(local_vim)
		local_vim.opt.relativenumber = true
		local_vim.g.mapleader = " "
		--   local_vim.opt.whichwrap = vim.opt.whichwrap - { 'b', 's' } -- removing option from list
		--   local_vim.opt.shortmess = vim.opt.shortmess + { I = true } -- add to option list
		--
		return local_vim
	end,

	-- Set dashboard header
	header = {
		-- 	"██   ██  ███████  ██████    ██████",
		-- 	" ██ ██   ██       ██   ██  ██    ██",
		-- 	"  ███    ███████  ██████   ██    ██",
		-- 	" ██ ██   ██       ██   ██  ██    ██",
		-- 	"██   ██  ███████  ██   ██   ██████",
		-- 	" ",
		"██╗░░██╗███████╗██████╗░░█████╗░",
		"╚██╗██╔╝██╔════╝██╔══██╗██╔══██╗",
		"░╚███╔╝░█████╗░░██████╔╝██║░░██║",
		"░██╔██╗░██╔══╝░░██╔══██╗██║░░██║",
		"██╔╝╚██╗███████╗██║░░██║╚█████╔╝",
		"╚═╝░░╚═╝╚══════╝╚═╝░░╚═╝░╚════╝░",

		-- "─────────────────────────────────────────────────────────────────────",
		-- "─████████──████████─██████████████─████████████████───██████████████─",
		-- "─██░░░░██──██░░░░██─██░░░░░░░░░░██─██░░░░░░░░░░░░██───██░░░░░░░░░░██─",
		-- "─████░░██──██░░████─██░░██████████─██░░████████░░██───██░░██████░░██─",
		-- "───██░░░░██░░░░██───██░░██─────────██░░██────██░░██───██░░██──██░░██─",
		-- "───████░░░░░░████───██░░██████████─██░░████████░░██───██░░██──██░░██─",
		-- "─────██░░░░░░██─────██░░░░░░░░░░██─██░░░░░░░░░░░░██───██░░██──██░░██─",
		-- "───████░░░░░░████───██░░██████████─██░░██████░░████───██░░██──██░░██─",
		-- "───██░░░░██░░░░██───██░░██─────────██░░██──██░░██─────██░░██──██░░██─",
		-- "─████░░██──██░░████─██░░██████████─██░░██──██░░██████─██░░██████░░██─",
		-- "─██░░░░██──██░░░░██─██░░░░░░░░░░██─██░░██──██░░░░░░██─██░░░░░░░░░░██─",
		-- "─████████──████████─██████████████─██████──██████████─██████████████─",
		-- "─────────────────────────────────────────────────────────────────────",
	},

	-- Default theme configuration
	default_theme = {
		-- set the highlight style for diagnostic messages
		diagnostics_style = { italic = true },
		-- Modify the color palette for the default theme
		colors = {
			fg = "#abb2bf",
			bg = "#1e222a",
		},
		-- enable or disable highlighting for extra plugins
		plugins = {
			aerial = true,
			beacon = false,
			bufferline = true,
			dashboard = true,
			highlighturl = true,
			hop = false,
			indent_blankline = true,
			lightspeed = false,
			["neo-tree"] = true,
			notify = true,
			["nvim-tree"] = false,
			["nvim-web-devicons"] = true,
			rainbow = true,
			symbols_outline = false,
			telescope = true,
			vimwiki = false,
			["which-key"] = true,
		},
	},

	-- Diagnostics configuration (for vim.diagnostics.config({...}))
	diagnostics = {
		virtual_text = true,
		--underline = true,
		undercurl = true,
	},

	-- Extend LSP configuration
	lsp = {
		-- enable servers that you already have installed without mason
		servers = {
			-- "pyright"
		},
		-- easily add or disable built in mappings added during LSP attaching
		mappings = {
			n = {
				-- ["<leader>lf"] = false -- disable formatting keymap
			},
		},
		-- add to the global LSP on_attach function
		-- on_attach = function(client, bufnr)
		-- end,

		-- override the mason server-registration function
		-- server_registration = function(server, opts)
		--   require("lspconfig")[server].setup(opts)
		-- end,

		-- Add overrides for LSP server settings, the keys are the name of the server
		["server-settings"] = {
			-- example for addings schemas to yamlls
			-- yamlls = { -- override table for require("lspconfig").yamlls.setup({...})
			--   settings = {
			--     yaml = {
			--       schemas = {
			--         ["http://json.schemastore.org/github-workflow"] = ".github/workflows/*.{yml,yaml}",
			--         ["http://json.schemastore.org/github-action"] = ".github/action.{yml,yaml}",
			--         ["http://json.schemastore.org/ansible-stable-2.9"] = "roles/tasks/*.{yml,yaml}",
			--       },
			--     },
			--   },
			-- },
		},
	},

	-- Mapping data with "desc" stored directly by vim.keymap.set().
	--
	-- Please use this mappings table to set keyboard mapping since this is the
	-- lower level configuration and more robust one. (which-key will
	-- automatically pick-up stored data by this setting.)
	mappings = {
		-- first key is the mode
		n = {
			-- second key is the lefthand side of the map
			-- mappings seen under group name "Buffer"
			["<leader>bb"] = { "<cmd>tabnew<cr>", desc = "New tab" },
			["<leader>bc"] = { "<cmd>BufferLinePickClose<cr>", desc = "Pick to close" },
			["<leader>bj"] = { "<cmd>BufferLinePick<cr>", desc = "Pick to jump" },
			["<leader>bt"] = { "<cmd>BufferLineSortByTabs<cr>", desc = "Sort by tabs" },
			-- quick save
			-- ["<C-s>"] = { ":w!<cr>", desc = "Save File" },  -- change description but the same command
		},
		t = {
			-- setting a mapping to false will disable it
			-- ["<esc>"] = false,
		},
	},

	-- Configure plugins
	plugins = {
		init = {
			{
				require("gruvbox").setup({
					undercurl = true,
					bold = true,
					italic = true,
					strikethrough = true,
					invert_selection = false,
					invert_signs = false,
					invert_tabline = false,
					invert_intend_guides = false,
					inverse = true, -- invert background for search, diffs, statuslines and errors
					contrast = "hard", -- can be "hard", "soft" or empty string
					transparent_mode = true,
					overrides = {
						DiagnosticVirtualTextError = { fg = "#fb4934", bg = "#400404" },
						DiagnosticVirtualTextWarn = { fg = "#fabd2f", bg = "#3f4004" },
						DiagnosticVirtualTextInfo = { fg = "#83a598", bg = "#040540" },
						DiagnosticVirtualTextHint = { fg = "#427b58", bg = "#043d40" },
						BufferLineBufferSelected = { fg = "#637CF7", bold = true },
					},
				}),
				vim.cmd("colorscheme gruvbox"),
				require("notify").setup({
					background_colour = "Normal",
					opacity = 20,
					timeout = 3000,
				}),
			},

			require("treesitter-context").setup({
				enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
				max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
				trim_scope = "outer", -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
				patterns = { -- Match patterns for TS nodes. These get wrapped to match at word boundaries.
					-- For all filetypes
					-- Note that setting an entry here replaces all other patterns for this entry.
					-- By setting the 'default' entry below, you can control which nodes you want to
					-- appear in the context window.
					default = {
						"class",
						"function",
						"method",
						"for",
						"while",
						"if",
						"switch",
						"case",
					},
					-- Patterns for specific filetypes
					-- If a pattern is missing, *open a PR* so everyone can benefit.
					tex = {
						"chapter",
						"section",
						"subsection",
						"subsubsection",
					},
					rust = {
						"impl_item",
						"struct",
						"enum",
					},
					scala = {
						"object_definition",
					},
					vhdl = {
						"process_statement",
						"architecture_body",
						"entity_declaration",
					},
					markdown = {
						"section",
					},
					elixir = {
						"anonymous_function",
						"arguments",
						"block",
						"do_block",
						"list",
						"map",
						"tuple",
						"quoted_content",
					},
					json = {
						"pair",
					},
					yaml = {
						"block_mapping_pair",
					},
				},
				exact_patterns = {
					-- Example for a specific filetype with Lua patterns
					-- Treat patterns.rust as a Lua pattern (i.e "^impl_item$" will
					-- exactly match "impl_item" only)
					-- rust = true,
				},
			}),
		},
		-- All other entries override the require("<key>").setup({...}) call for default plugins
		["null-ls"] = function(config) -- overrides `require("null-ls").setup(config)`
			-- config variable is the default configuration table for the setup functino call
			local null_ls = require("null-ls")
			config.sources = {
				-- Set a formatter
				null_ls.builtins.formatting.stylua,
				null_ls.builtins.formatting.prettier,
				null_ls.builtins.formatting.autopep8,
				null_ls.builtins.formatting.djlint,
				null_ls.builtins.formatting.goimports,
				null_ls.builtins.formatting.clang_format,
			}
			-- set up null-ls's on_attach function
			-- NOTE: You can remove this on attach function to disable format on save
			config.on_attach = function(client)
				if client.resolved_capabilities.document_formatting then
					vim.api.nvim_create_autocmd("BufWritePre", {
						desc = "Auto format before save",
						pattern = "<buffer>",
						callback = vim.lsp.buf.formatting_sync,
					})
				end
			end
			return config -- return final config table to use in require("null-ls").setup(config)
		end,
		treesitter = { -- overrides `require("treesitter").setup(...)`
			ensure_installed = {
				"lua",
				"python",
				"javascript",
				"typescript",
				"go",
				"c_sharp",
				"rust",
				"toml",
				"tsx",
				"css",
				"comment",
			},
		},
		-- use mason-lspconfig to configure LSP installations
		["mason-lspconfig"] = { -- overrides `require("mason-lspconfig").setup(...)`
			ensure_installed = {
				"sumneko_lua",
				"pyright",
				"gopls",
				"eslint-lsp",
				"omnisharp",
				"rust-analyzer",
				"taplo",
				"css-lsp",
			},
		},
		-- use mason-tool-installer to configure DAP/Formatters/Linter installation
		["mason-tool-installer"] = { -- overrides `require("mason-tool-installer").setup(...)`
			ensure_installed = {
				"prettier",
				"stylua",
				"autopep8",
				"flake8",
				"revive",
				"goimports",
				"eslint_d",
				"csharpier",
				"djlint",
			},
		},
		packer = { -- overrides `require("packer").setup(...)`
			compile_path = vim.fn.stdpath("data") .. "/packer_compiled.lua",
		},
	},

	-- LuaSnip Options
	luasnip = {
		-- Add paths for including more VS Code style snippets in luasnip
		vscode_snippet_paths = {},
		-- Extend filetypes
		filetype_extend = {
			javascript = { "javascriptreact" },
		},
	},

	-- CMP Source Priorities
	-- modify here the priorities of default cmp sources
	-- higher value == higher priority
	-- The value can also be set to a boolean for disabling default sources:
	-- false == disabled
	-- true == 1000
	cmp = {
		source_priority = {
			nvim_lsp = 1000,
			luasnip = 750,
			buffer = 500,
			path = 250,
		},
	},

	-- Modify which-key registration (Use this with mappings table in the above.)
	["which-key"] = {
		-- Add bindings which show up as group name
		register_mappings = {
			-- first key is the mode, n == normal mode
			n = {
				-- second key is the prefix, <leader> prefixes
				["<leader>"] = {
					-- third key is the key to bring up next level and its displayed
					-- group name in which-key top level menu
					["b"] = { name = "Buffer" },
				},
			},
		},
	},

	-- This function is run last and is a good place to configuring
	-- augroups/autocommands and custom filetypes also this just pure lua so
	-- anything that doesn't fit in the normal config locations above can go here
	polish = function()
		-- Set key binding
		-- Set autocommands
		vim.api.nvim_create_augroup("packer_conf", { clear = true })
		vim.api.nvim_create_autocmd("BufWritePost", {
			desc = "Sync packer after modifying plugins.lua",
			group = "packer_conf",
			pattern = "plugins.lua",
			command = "source <afile> | PackerSync",
		})

		-- Set up custom filetypes
		-- vim.filetype.add {
		--   extension = {
		--     foo = "fooscript",
		--   },
		--   filename = {
		--     ["Foofile"] = "fooscript",
		--   },
		--   pattern = {
		--     ["~/%.config/foo/.*"] = "fooscript",
		--   },
		-- }
	end,
}

return config
