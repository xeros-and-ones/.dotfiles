---@type NvPluginSpec[]
local plugins = {
	-- Autocompletion
	{
		"hrsh7th/nvim-cmp",
		version = false,
		event = { "InsertEnter", "CmdlineEnter" },
		opts = function()
			local cmp = require("cmp")
			cmp.setup.cmdline(":", {
				sources = {
					{ name = "cmdline", keyword_length = 1 },
					{ name = "path" },
					{ name = "nvim_lua" },
				},
				cmp.setup.cmdline({ "/", "?" }, {
					sources = {
						{ name = "buffer", keyword_length = 1 },
					},
				}),
			})
		end,
		config = function(_, opts)
			table.insert(opts.sources, 6, { name = "crates" })
			local cmp = require("cmp")
			local luasnip = require("luasnip")
			local has_words_before = function()
				if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
					return false
				end
				local line, col = unpack(vim.api.nvim_win_get_cursor(0))
				return col ~= 0
					and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match("^%s*$") == nil
			end
			opts.mapping = vim.tbl_extend("force", {}, opts.mapping, {
				["<Up>"] = cmp.mapping(
					cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
					{ "i", "c" }
				),
				["<Down>"] = cmp.mapping(
					cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
					{ "i", "c" }
				),
				["<C-k>"] = cmp.mapping(
					cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
					{ "i", "c" }
				),
				["<C-j>"] = cmp.mapping(
					cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
					{ "i", "c" }
				),
				["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
				["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
				["<c-space>"] = cmp.mapping({
					i = cmp.mapping.complete(),
					c = function(_) -- fallback
						if cmp.visible() then
							if not cmp.confirm({ select = true }) then
								return
							end
						else
							cmp.complete()
						end
					end,
				}),
				["<C-y>"] = cmp.mapping(
					cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
					{ "i", "c" }
				),
				["<C-e>"] = cmp.mapping.close(),
				["<CR>"] = cmp.mapping.confirm({
					behavior = cmp.ConfirmBehavior.Replace,
					select = true,
				}),
				["<Tab>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_next_item()
					elseif luasnip.expandable() then
						luasnip.expand()
					elseif luasnip.expand_or_jumpable() then
						luasnip.expand_or_jump()
					elseif require("neogen").jumpable(1) then
						require("neogen").jump_next()
					elseif has_words_before() then
						cmp.complete()
					else
						fallback()
					end
				end, {
					"i",
					"s",
				}),
				["<S-Tab>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_prev_item()
					elseif luasnip.jumpable(-1) then
						luasnip.jump(-1)
					elseif require("neogen").jumpable(-1) then
						require("neogen").jump_prev()
					else
						fallback()
					end
				end, {
					"i",
					"s",
				}),
			})
			opts.experimental = {
				ghost_text = true,
				native_menu = false,
			}
			opts.sorting = {
				comparators = {
					cmp.config.compare.offset,
					cmp.config.compare.exact,
					cmp.config.compare.score,
					function(entry1, entry2)
						local _, entry1_under = entry1.completion_item.label:find("^_+")
						local _, entry2_under = entry2.completion_item.label:find("^_+")
						entry1_under = entry1_under or 0
						entry2_under = entry2_under or 0
						if entry1_under > entry2_under then
							return false
						elseif entry1_under < entry2_under then
							return true
						end
					end,
					cmp.config.compare.kind,
					cmp.config.compare.sort_text,
					cmp.config.compare.length,
					cmp.config.compare.order,
				},
			}
			opts.window.documentation.winhighlight = "Normal:CmpPmenu"
			opts.window.completion.scrollbar = true

			opts.completion["completeopt"] = "menu,menuone,noinsert"
			require("cmp").setup(opts)
		end,
		dependencies = {
			{
				"hrsh7th/cmp-cmdline",
			},
			{ -- Luasnip
				"L3MON4D3/LuaSnip",
				dependencies = "rafamadriz/friendly-snippets",
				opts = {
					history = true,
					update_events = "TextChanged,TextChangedI",
					delete_check_events = "TextChanged,InsertLeave",
					enable_autosnippets = true,
				},
				config = function(_, opts)
					require("plugins.configs.others").luasnip(opts)
					require("luasnip.loaders.from_vscode").lazy_load({
						require("luasnip").filetype_extend("python", { "django" }),
						require("luasnip").filetype_extend("python", { "pydoc" }),
						require("luasnip").filetype_extend("javascript", { "html" }),
						require("luasnip").filetype_extend("javascript", { "javascriptreact" }),
						require("luasnip").filetype_extend("htmldjango", { "djangohtml" }),
						require("luasnip").filetype_extend("htmldjango", { "html" }),
					})
				end,
			},
		},
	},
	------------------------------------------------------------------------------------------
	{
		"danymat/neogen",
		enabled = true,
		event = { "BufRead", "BufNewFile" },
		opts = {
			snippet_engine = "luasnip",
			languages = {
				python = {
					template = {
						annotation_convention = "google_docstrings",
					},
				},
			},
		},
	},
	------------------------------------------------------------------------------------------
	{
		"utilyre/barbecue.nvim",
		name = "barbecue",
		version = "*",
		dependencies = {
			"SmiteshP/nvim-navic",
			"nvim-tree/nvim-web-devicons", -- optional dependency
		},
		opts = {
			-- configurations go here
		},
		lazy = false,
	},
	------------------------------------------------------------------------------------------
	-- Utilities
	{
		"nvim-lua/plenary.nvim",
	},
	-------------------------------------------------------------------------------
	{ "tenxsoydev/karen-yank.nvim", lazy = false, config = true },
	------------------------------------------------------------------------------------------
	-- Native LSP
	{
		"neovim/nvim-lspconfig",
		config = function()
			require("custom.configs.lspconfig")
		end,
		dependencies = {
			-- Formatting
			{ "microsoft/python-type-stubs" },
			{
				"nvimtools/none-ls.nvim",
				event = "BufReadPre",
				config = function()
					require("custom.configs.none_ls")
				end,
			},
			{
				"mrcjkb/rustaceanvim",
				version = "^3", -- Recommended
				init = function()
					-- Configure rustaceanvim here
					vim.g.rustaceanvim = {}
				end,
				ft = { "rust" },
			},
			-- Installer
			{
				"williamboman/mason.nvim",
				cmd = {
					"Mason",
					"MasonInstall",
					"MasonUpdate",
					"MasonUninstall",
					"MasonUninstallAll",
					"MasonLog",
				},
				opts = require("custom.configs.mason"),
			},
			{
				"williamboman/mason-lspconfig.nvim",
			},
			-- Improve Other LSP Functionalities
			{
				"nvimdev/lspsaga.nvim",
				opts = require("custom.configs.lspsaga"),
			},
			-- For Plugin Development
			{
				"folke/neodev.nvim",
				enabled = true,
				opts = require("custom.configs.neodev"),
			},
		},
	},
	{
		"ziglang/zig.vim",
	},
	------------------------------------------------------------------------------------------
	-- File Explorer
	{
		"nvim-tree/nvim-tree.lua",
		enabled = true,
		init = function()
			require("core.utils").load_mappings("Nvimtree")
		end,
		cmd = {
			"NvimTreeOpen",
			"NvimTreeToggle",
			"NvimTreeFocus",
			"NvimTreeFindFile",
			"NvimTreeFindFileToggle",
		},
		opts = function()
			return require("custom.configs.nvimtree")
		end,
		config = function(_, opts)
			dofile(vim.g.base46_cache .. "nvimtree")
			require("nvim-tree").setup(opts)
		end,
	},
	------------------------------------------------------------------------------------------
	{
		"echasnovski/mini.move",
		event = "BufRead",
		opts = {
			-- Module mappings. Use `''` (empty string) to disable one.
			mappings = {
				-- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
				left = "<C-S-h>",
				right = "<C-S-l>",
				down = "<C-S-j>",
				up = "<C-S-k>",

				-- Move current line in Normal mode
				line_left = "<C-S-h>",
				line_right = "<C-S-l>",
				line_down = "<C-S-j>",
				line_up = "<C-S-k>",
			},

			-- Options which control moving behavior
			options = {
				-- Automatically reindent selection during linewise vertical move
				reindent_linewise = true,
			},
		},
	},
	------------------------------------------------------------------------------------------
	{
		"echasnovski/mini.splitjoin",
		event = "BufRead",
		opts = {
			mappings = {
				toggle = "gj",
				split = "",
				join = "",
			},
		},
	},
	------------------------------------------------------------------------------------------
	{
		"echasnovski/mini.surround",
		event = "BufRead",
		opts = {
			mappings = {
				add = "ga", -- Add surrounding in Normal and Visual modes
				delete = "gd", -- Delete surrounding
				find = "", -- Find surrounding (to the right)
				find_left = "", -- Find surrounding (to the left)
				highlight = "", -- Highlight surrounding
				replace = "gr", -- Replace surrounding
				update_n_lines = "", -- Update `n_lines`
			},
		},
	},
	-----------------------------------------------------------------
	{
		"lukas-reineke/indent-blankline.nvim",
		version = "",
		main = "ibl",
		init = function()
			require("core.utils").lazy_load("indent-blankline.nvim")
		end,
		config = function()
			dofile(vim.g.base46_cache .. "blankline")
			require("ibl").setup({
				exclude = {
					filetypes = {
						"help",
						"terminal",
						"lazy",
						"lspinfo",
						"TelescopePrompt",
						"TelescopeResults",
						"mason",
						"nvdash",
						"nvcheatsheet",
					},
					buftypes = { "terminal" },
				},
				indent = { char = "▏", smart_indent_cap = true },
				scope = {
					enabled = true,
					show_start = true,
					show_end = false,
					injected_languages = true,
				},
			})
		end,
	},
	------------------------------------------------------------------------------
	{
		"anuvyklack/hydra.nvim",
		event = "VeryLazy",
		config = function()
			require("custom.configs.hydra")
		end,
	},
	-------------------------------------------------------------------------------
	{
		"folke/persistence.nvim",
		event = "BufReadPre",
		opts = {
			dir = vim.fn.expand(vim.fn.stdpath("state") .. "/sessions/"), -- directory where session files are saved
			options = { "buffers", "curdir", "folds", "tabpages", "winsize", "globals" }, -- sessionoptions used for saving
			pre_save = nil, -- a function to call before saving the session
		},
	},

	-------------------------------------------------------------------------------
	{
		"gbprod/stay-in-place.nvim",
		enabled = true,
		event = { "BufReadPost", "BufNewFile" },
		opts = { set_keymaps = true, preserve_visual_selection = true },
	},

	-----------------------------------------------------------------
	{
		"RRethy/vim-illuminate",
		enabled = true,
		event = { "CursorHold", "CursorHoldI" },
		config = function()
			require("illuminate").configure({
				under_cursor = true,
				delay = 700,
				providers = {
					"lsp",
					"treesitter",
					"regex",
				},
				filetypes_denylist = {
					"NvimTree",
					"Trouble",
					"Outline",
					"TelescopePrompt",
					"Empty",
					"dirvish",
					"fugitive",
					"alpha",
					"packer",
					"neogitstatus",
					"spectre_panel",
					"toggleterm",
					"DressingSelect",
					"aerial",
				},
			})
		end,
	},
	------------------------------------------------------------------------------------------
	-- Improve Folds
	{
		"kevinhwang91/nvim-ufo",
		event = "VeryLazy",
		enabled = true,
		dependencies = {
			"kevinhwang91/promise-async",
			{
				"luukvbaal/statuscol.nvim",
				event = "BufReadPost",
				config = function()
					local builtin = require("statuscol.builtin")
					require("statuscol").setup({
						relculright = true,
						segments = {
							-- {
							-- 	sign = { name = { "Dap*" }, namespace = { "bulb*" } },
							-- 	click = "v:lua.ScSa",
							-- },
							{
								sign = { name = { ".*" }, maxwidth = 1, colwidth = 1 },
								click = "v:lua.ScSa",
							},
							{
								sign = { name = { "Diagnostic" }, maxwidth = 2 },
								click = "v:lua.ScSa",
							},
							{ text = { builtin.lnumfunc }, click = "v:lua.ScLa" },
							{ text = { builtin.foldfunc, " " }, click = "v:lua.ScFa" },
							{
								sign = { namespace = { "gitsign*" }, colwidth = 1 },
								click = "v:lua.ScSa",
							},
						},
					})
				end,
			},
		},

		config = function()
			require("custom.configs.folds")
		end,
	},
	-------------------------------------------------------------------------------
	-- Syntax Highlighting
	{
		"nvim-treesitter/nvim-treesitter",
		init = function()
			require("core.utils").lazy_load("nvim-treesitter")
		end,
		cmd = {
			"TSInstall",
			"TSUninstall",
			"TSInstallInfo",
			"TSUpdate",
			"TSBufEnable",
			"TSBufDisable",
			"TSEnable",
			"TSDisable",
			"TSModuleInfo",
			"TSToggle",
			"TSBufToggle",
		},
		opts = require("custom.configs.treesitter"),
		config = function(_, opts)
			dofile(vim.g.base46_cache .. "syntax")
			require("nvim-treesitter.configs").setup(opts)
		end,
		dependencies = {
			"windwp/nvim-ts-autotag",
			"nvim-treesitter/nvim-treesitter-textobjects",
		},
	},
	-------------------------------------------------------------------------------
	-- Schemas
	{ "b0o/schemastore.nvim" },
	-------------------------------------------------------------------------------
	-- Buffer Delete
	{
		"moll/vim-bbye",
		cmd = { "Bdelete", "Bwipeout" },
	},
	-------------------------------------------------------------------------------
	-- Highlight, List and Search Todo comments in your projects
	{
		"folke/todo-comments.nvim",
		event = "VeryLazy",
		cmd = { "TodoTrouble", "TodoLocList", "TodoQuickFix", "TodoTelescope" },
		opts = require("custom.configs.todo-comments"),
	},
	-------------------------------------------------------------------------------
	-- NvChad Built-in Terminal
	{
		"NvChad/nvterm",
		enabled = false,
	},
	-------------------------------------------------------------------------------
	{
		"nvim-neotest/neotest",
		init = function()
			require("core.utils").load_mappings("Neotest")
		end,
		dependencies = {
			"nvim-neotest/neotest-python",
			"nvim-neotest/neotest-go",
			"rouge8/neotest-rust",
			"Issafalcon/neotest-dotnet",
		},
		config = function()
			require("custom.configs.neotest")
		end,
	},
	-------------------------------------------------------------------------------
	-----------------  Compiler
	-- { -- This plugin
	-- 	"Zeioth/compiler.nvim",
	-- 	cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
	-- 	dependencies = { "stevearc/overseer.nvim" },
	-- 	opts = {},
	-- },
	-- { -- The task runner we use
	-- 	"stevearc/overseer.nvim",
	-- 	commit = "400e762648b70397d0d315e5acaf0ff3597f2d8b",
	-- 	cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
	-- 	opts = {
	-- 		task_list = {
	-- 			direction = "bottom",
	-- 			min_height = 25,
	-- 			max_height = 25,
	-- 			default_detail = 1,
	-- 		},
	-- 	},
	-- },
	-------------------------------------------------------------------------------
	-- Improve UI
	{
		"stevearc/dressing.nvim",
		event = "VeryLazy",
		opts = require("custom.configs.dressing"),
	},
	-------------------------------------------------------------------------------
	-- Search motions
	{
		"folke/flash.nvim",
		event = "VeryLazy",
		opts = require("custom.configs.flash"),
	},
	-------------------------------------------------------------------------------
	-- Fuzzy Finder
	{
		"nvim-telescope/telescope.nvim",
		opts = function()
			return require("custom.configs.telescope")
		end,
		config = function(_, opts)
			dofile(vim.g.base46_cache .. "telescope")
			local telescope = require("telescope")
			telescope.setup(opts)
			local extensions_list = { "themes", "terms", "fzf", "project", "undo" }

			-- load extensions
			for _, ext in ipairs(extensions_list) do
				telescope.load_extension(ext)
			end
		end,
		dependencies = {
			"nvim-telescope/telescope-project.nvim",
			"debugloop/telescope-undo.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},
	},
	-------------------------------------------------------------------------------
	-- Pretty Diagnostics and Lists
	{
		"folke/trouble.nvim",
		cmd = { "TroubleToggle", "Trouble" },
		opts = require("custom.configs.trouble"),
	},
	-------------------------------------------------------------------------------
	-- Git Signs
	{
		"lewis6991/gitsigns.nvim",
		init = function()
			require("core.utils").load_mappings("Git")
			-- load gitsigns only when a git file is opened
			vim.api.nvim_create_autocmd({ "BufRead" }, {
				group = vim.api.nvim_create_augroup("GitSignsLazyLoad", { clear = true }),
				callback = function()
					vim.fn.system("git -C " .. '"' .. vim.fn.expand("%:p:h") .. '"' .. " rev-parse")
					if vim.v.shell_error == 0 then
						vim.api.nvim_del_augroup_by_name("GitSignsLazyLoad")
						vim.schedule(function()
							require("lazy").load({ plugins = { "gitsigns.nvim" } })
						end)
					end
				end,
			})
		end,

		opts = {
			signs = {
				add = { text = "│" },
				change = { text = "│" },
				delete = { text = "│" },
				topdelete = { text = "‾" },
				changedelete = { text = "~" },
				untracked = { text = "┆" },
			},
		},
		-- Show diffs
		dependencies = {
			{
				"sindrets/diffview.nvim",
				event = "VeryLazy",
				cmd = {
					"DiffviewOpen",
					"DiffviewFocusFiles",
					"DiffviewToggleFiles",
					"DiffviewClose",
					"DiffviewRefresh",
					"DiffviewToggle",
					"DiffviewFileHistory",
				},
				config = function()
					require("custom.configs.diffview")
				end,
			},
		},
	},

	-----------------------------------------------------------------

	-- Debugging
	{
		"mfussenegger/nvim-dap",
		event = "VeryLazy",
		config = function()
			require("custom.configs.dap").dap()
		end,
		dependencies = {
			{
				"theHamsta/nvim-dap-virtual-text",
				config = function()
					require("nvim-dap-virtual-text").setup()
				end,
			},
			{
				"rcarriga/nvim-dap-ui",
				config = function()
					require("custom.configs.dap").dapui()
				end,
			},
		},
	},
	-----------------------------------------------------------------
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function() end,
		cmd = "WhichKey",
		opts = require("custom.configs.whichkey"),
		config = function(_, opts)
			dofile(vim.g.base46_cache .. "whichkey")
			require("which-key").setup(opts)
			require("which-key").register({
				b = { name = "Buffers" },
				c = { name = "Code" },
				D = { name = "Dadbod Database" },
				f = { name = "Find" },
				g = { name = "Git Control" },
				n = { name = "Neovim" },
				o = { name = "Options" },
				s = { name = "Spectre" },
				z = { name = "Utilities" },
				t = { name = "Testing" },
				l = { name = "LSP" },
			}, { prefix = "<leader>" })
			require("which-key").register({
				s = { name = "Surround" },
			}, { prefix = "g" })
		end,
	},
	-----------------------------------------------------------------
	-- UI for messages, cmdline, and popup
	{
		"folke/noice.nvim",
		opts = require("custom.configs.noice"),
		config = function(_, opts)
			require("noice").setup(opts)
			vim.keymap.set({ "n", "i", "s" }, "<c-d>", function()
				if not require("noice.lsp").scroll(2) then
					return "<c-d>"
				end
			end, { silent = true, expr = true })

			vim.keymap.set({ "n", "i", "s" }, "<c-f>", function()
				if not require("noice.lsp").scroll(-2) then
					return "<c-f>"
				end
			end, { silent = true, expr = true })
		end,
		dependencies = "MunifTanjim/nui.nvim",
	},
	-----------------------------------------------------------------
	{
		"max397574/better-escape.nvim",
		event = "BufReadPre",
		config = function()
			require("better_escape").setup({ mapping = "jk" })
		end,
	},
}

return plugins
