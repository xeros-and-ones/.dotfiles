---@type MappingsTable
local M = {}
local fn = vim.fn
local cwd = vim.fn.stdpath("config") .. "/"
local config_dir = { cwd }

M.Neotree = {
	plugin = true,
	n = {
		["<leader>e"] = { "<cmd>Neotree toggle<cr>", "File Explorer", opts = { silent = true } },
	},
}
local nvimTreeFocusOrToggle = function()
	local nvimTree = require("nvim-tree.api")
	local currentBuf = vim.api.nvim_get_current_buf()
	local currentBufFt = vim.api.nvim_get_option_value("filetype", { buf = currentBuf })
	if currentBufFt == "NvimTree" then
		nvimTree.tree.toggle()
	else
		nvimTree.tree.focus()
	end
end

M.Nvimtree = {
	plugin = true,
	n = {
		["<leader>e"] = { nvimTreeFocusOrToggle, "File Explorer" },
	},
}

M.Neotest = {
	plugin = true,
	n = {
		["<leader>tt"] = {
			function()
				vim.cmd('lua require("neotest").run.run(vim.fn.expand "%")')
			end,
			"Run File",
			opts = { silent = true },
		},
		["<leader>tT"] = {
			function()
				vim.cmd('lua require("neotest").run.run(vim.loop.cwd())')
			end,
			"Run All Test Files",
			opts = { silent = true },
		},
		["<leader>tr"] = {
			function()
				vim.cmd('lua require("neotest").run.run()')
			end,
			"Run Nearest",
			opts = { silent = true },
		},
		["<leader>td"] = {
			function()
				vim.cmd('lua require("neotest").run.run { strategy = "dap" }')
			end,
			"Run Dap",
			opts = { silent = true },
		},
		["<leader>ts"] = {
			function()
				vim.cmd('lua require("neotest").summary.toggle()')
			end,
			"Toggle Summary",
			opts = { silent = true },
		},
		["<leader>to"] = {
			function()
				vim.cmd('lua require("neotest").output.open { enter = true, auto_close = true }')
			end,
			"Show Output",
			opts = { silent = true },
		},
		["<leader>tO"] = {
			function()
				vim.cmd('lua require("neotest").output_panel.toggle()')
			end,
			"Toggle Output Panel",
			opts = { silent = true },
		},
		["<leader>tS"] = {
			function()
				vim.cmd('lua require("neotest").run.stop()')
			end,
			"Stop",
			opts = { silent = true },
		},
	},
}

M.Neovim = {
	n = {
		["<leader>nf"] = {
			function()
				require("telescope.builtin").find_files({
					follow = true,
					hidden = true,
					no_ignore = true,
					prompt_title = "Config Files",
					search_dirs = config_dir,
					cwd = cwd,
				})
			end,
			"Find Config Files",
			opts = { silent = true },
		},
		["<leader>ng"] = {
			function()
				require("telescope.builtin").live_grep({
					prompt_title = "Config Files",
					search_dirs = config_dir,
					cwd = cwd,
				})
			end,
			"Grep Config Files",
			opts = { silent = true },
		},
		["<leader>nc"] = {
			"<cmd>NvCheatsheet<cr>",
			"Cheatsheet",
			opts = { silent = true },
		},
		["<leader>ni"] = { "<cmd>Inspect<CR>", "TS Inspect", opts = { silent = true } },
		["<leader>nI"] = { "<cmd>InspectTree<CR>", "TS Inspect Tree", opts = { silent = true } },
		["<leader>nm"] = { "<cmd>messages<cr>", "Messages", opts = { silent = true } },
		["<leader>nh"] = { "<cmd>checkhealth<cr>", "Health", opts = { silent = true } },
		["<Leader>na"] = { "<cmd>Nvdash<cr>", "NvDash", opts = { silent = true } },
		["<leader>nv"] = {
			function()
				local version = vim.version().major .. "." .. vim.version().minor .. "." .. vim.version().patch
				return vim.notify(version, vim.log.levels.INFO, { title = "Neovim Version" })
			end,
			"Version",
			opts = { silent = true },
		},
	},
}

M.Options = {
	n = {
		["<leader>ol"] = { "<cmd>set nu!<cr>", "Toggle line number", opts = { silent = true } },
		["<leader>or"] = { "<cmd>set rnu!<cr>", "Toggle relative number", opts = { silent = true } },
		["<leader>ot"] = {
			function()
				require("base46").toggle_transparency()
			end,
			"Toggle Transparency",
			opts = { silent = true },
		},
		["<leader>oL"] = { "<cmd>Lazy<cr>", "Lazy", opts = { silent = true } },
		["<leader>om"] = { "<cmd>Mason<cr>", "Mason", opts = { silent = true } },
		["<leader>oi"] = { "<cmd>LspInfo<cr>", "LspInfo", opts = { silent = true } },
		["<leader>on"] = { "<cmd>NullLsInfo<cr>", "Null-LS", opts = { silent = true } },
		["<leader>os"] = {
			function()
				require("persistence").load()
			end,
			"Restore Last Session",
		},
	},
}

M.Tools = {
	n = {
		["<leader>zc"] = { "<cmd>CccPick<cr>", "Colour picker", opts = { silent = true } },
		["<leader>zs"] = {
			"<cmd>lua require('luasnip.loaders').edit_snippet_files()<cr>",
			"Edit snippets",
			opts = { silent = true },
		},

		["<leader>zO"] = { "<cmd>OverseerToggle<cr>", "Overseer List", opts = { silent = true } },
		["<leader>zo"] = { "<cmd>OverseerRun<cr>", "Overseer Run", opts = { silent = true } },
		["<Leader>zd"] = { "<cmd>lua require('neogen').generate()<CR>", "Generate docs", opts = { silent = true } },
	},
}

M.Telescope = {
	n = {
		["<leader>fu"] = { "<CMD>Telescope undo<CR>", "Undo History", opts = { silent = true } },
		["<leader>fa"] = { "<cmd>Telescope autocommands<cr>", "Autocommmands", opts = { silent = true } },
		["<leader>ff"] = {
			function()
				require("telescope.builtin").find_files({ follow = true, hidden = true, no_ignore = true })
			end,
			"Find Files",
			opts = { silent = true },
		},
		["<leader>fm"] = { "<cmd>Telescope marks<cr>", "Marks", opts = { silent = true } },
		["<leader>fw"] = {
			function()
				require("telescope.builtin").live_grep({
					additional_args = function(args)
						return vim.list_extend(args, { "--hidden", "--no-ignore" })
					end,
				})
			end,
			"Word",
			opts = { silent = true },
		},
		["<leader>ft"] = { "<cmd>Telescope themes<cr>", "Themes", opts = { silent = true } },
		-- B = { "<cmd>Telescope bookmarks<cr>", "Browswer Bookmarks" , opts = { silent = true }},
		["<leader>fb"] = { "<cmd>Telescope buffers<cr>", "Buffers", opts = { silent = true } },
		["<leader>fn"] = {
			"<cmd>lua require('telescope').extensions.notify.notify()<cr>",
			"Notify History",
			opts = { silent = true },
		},
		["<leader>fp"] = { "<cmd>Telescope projects<cr>", "Projects", opts = { silent = true } },
		-- s = { "<cmd>Telescope persisted<cr>", "Sessions" , opts = { silent = true }},
		["<leader>fh"] = { "<cmd>Telescope help_tags<cr>", "Help", opts = { silent = true } },
		["<leader>fk"] = { "<cmd>Telescope keymaps<cr>", "Keymaps", opts = { silent = true } },
		["<leader>fC"] = { "<cmd>Telescope commands<cr>", "Commands", opts = { silent = true } },
		["<leader>fr"] = { "<cmd>Telescope oldfiles<cr>", "Recent Files", opts = { silent = true } },
		["<leader>fH"] = { "<cmd>Telescope highlights<cr>", "Highlights", opts = { silent = true } },
		["<leader>fT"] = { "<cmd>TodoTrouble<CR>", "TODO", opts = { silent = true } },
		["<leader>fi"] = { "<cmd>Telescope terms<CR>", "Pick hidden term", opts = { silent = true } },
		["<leader>fo"] = { "<cmd>Telescope registers<CR>", "Search registers", opts = { silent = true } },
		["<leader>f/"] = { "<cmd>Telescope current_buffer_fuzzy_find<CR>", "Find in buffer", opts = { silent = true } },
		["<leader>fc"] = {
			function()
				require("telescope.builtin").grep_string()
			end,
			"Cursor Word",
			opts = { silent = true },
		},
		["<leader>fM"] = {
			function()
				require("telescope.builtin").man_pages()
			end,
			"Man Pages",
			opts = { silent = true },
		},
	},
}

M.Toggleterm = {
	plugin = true,
	n = {
		["<c-\\>"] = { "<cmd>ToggleTerm direction=float<cr>", "float term", opts = { silent = true } },
		["<c-[>"] = { "<cmd>ToggleTerm size=20 direction=horizontal<cr>", "horizontal term", opts = { silent = true } },
		["<c-]>"] = { "<cmd>ToggleTerm size=70 direction=vertical<cr>", "vertical term", opts = { silent = true } },
	},
	t = {
		["<c-\\>"] = { "<cmd>ToggleTerm direction=float<cr>", "float term", opts = { silent = true } },
		["<c-[>"] = { "<cmd>ToggleTerm size=20 direction=horizontal<cr>", "horizontal term", opts = { silent = true } },
		["<c-]>"] = { "<cmd>ToggleTerm size=70 direction=vertical<cr>", "vertical term", opts = { silent = true } },
	},
}

M.UFO = {
	n = {
		["zR"] = {
			function()
				require("ufo").openAllFolds()
			end,
			"Open all folds",
			opts = { silent = true },
		},
		["zM"] = {
			function()
				require("ufo").closeAllFolds()
			end,
			"Close all folds",
			opts = { silent = true },
		},
		["zr"] = {
			function()
				require("ufo").openFoldsExceptKinds()
			end,
			"Open Folds",
			opts = { silent = true },
		},
		["zm"] = {
			function()
				require("ufo").closeFoldsWith()
			end,
			"close Folds",
			opts = { silent = true },
		},
		["zh"] = {
			function()
				require("ufo").peekFoldedLinesUnderCursor()
			end,
			"Fold preview",
			opts = { silent = true },
		},
	},
}

M.Dadbod = {
	plugin = true,
	n = {
		["<leader>Dt"] = { "<Cmd>DBUIToggle<Cr>", "Toggle UI", opts = { silent = true } },
		["<leader>Df"] = { "<Cmd>DBUIFindBuffer<Cr>", "Find buffer", opts = { silent = true } },
		["<leader>Dr"] = { "<Cmd>DBUIRenameBuffer<Cr>", "Rename buffer", opts = { silent = true } },
		["<leader>Dq"] = { "<Cmd>DBUILastQueryInfo<Cr>", "Last query info", opts = { silent = true } },
	},
}

local function show_hover()
	local filetype = vim.bo.filetype
	if vim.tbl_contains({ "vim", "help" }, filetype) then
		vim.cmd("h " .. vim.fn.expand("<cword>"))
	elseif vim.tbl_contains({ "man" }, filetype) then
		vim.cmd("Man " .. vim.fn.expand("<cword>"))
	elseif vim.fn.expand("%:t") == "Cargo.toml" and require("crates").popup_available() then
		require("crates").show_versions_popup()
	else
		vim.cmd("Lspsaga hover_doc")
	end
end

M.LSP = {
	n = {
		["<C-`>"] = { "<cmd>TroubleToggle<cr>", "Toggle Trouble", opts = { silent = true } },
		["<leader>cf"] = { "<cmd>NullFormat<cr>", "Format", opts = { silent = true } },
		["<leader>cr"] = {
			function()
				RunCode()
			end,
			"Run Code",
			opts = { silent = true },
		},
		["gh"] = { show_hover, "Hover Action", opts = { silent = true } },
		["<leader>cc"] = { "<cmd>CompilerOpen<cr>", "Compile", opts = { noremap = true, silent = true } },
		["<leader>cs"] = {
			"<cmd>CompilerStop | CompilerRedo<cr>",
			"Stop & Redo Compilation",
			opts = { noremap = true, silent = true },
		},
		["<leader>co"] = {
			"<cmd>CompilerToggleResults<cr>",
			"Compilation Results",
			opts = { noremap = true, silent = true },
		},
	},

	i = {
		["<C-`>"] = { "<cmd>TroubleToggle<cr>", "Toggle Trouble", opts = { silent = true } },
		["<leader>cf"] = { "<cmd>NullFormat<cr>", "Format", opts = { silent = true } },
		["<leader>cr"] = {
			function()
				RunCode()
			end,
			"Run Code",
			opts = { silent = true },
		},
	},

	v = {
		["<C-`>"] = { "<cmd>TroubleToggle<cr>", "Toggle Trouble", opts = { silent = true } },
		["<leader>cf"] = { "<cmd>NullFormat<cr>", "Format", opts = { silent = true } },
		["<leader>cr"] = {
			function()
				RunCode()
			end,
			"Run Code",
			opts = { silent = true },
		},
	},
}

M.Tabufline = {
	n = {
		-- cycle through buffers
		["L"] = {
			function()
				require("nvchad.tabufline").tabuflineNext()
			end,
			"Goto next buffer",
			opts = { silent = true },
		},

		["H"] = {
			function()
				require("nvchad.tabufline").tabuflinePrev()
			end,
			"Goto prev buffer",
			opts = { silent = true },
		},
		["<leader>bn"] = {
			function()
				require("nvchad.tabufline").tabuflineNext()
			end,
			"Goto next buffer",
			opts = { silent = true },
		},

		["<leader>bp"] = {
			function()
				require("nvchad.tabufline").tabuflinePrev()
			end,
			"Goto prev buffer",
			opts = { silent = true },
		},

		["<leader>b."] = {
			function()
				require("nvchad.tabufline").move_buf(1)
			end,
			"Move Buffer Right",
		},
		["<leader>b,"] = {
			function()
				require("nvchad.tabufline").move_buf(1)
			end,
			"Move Buffer left",
		},

		-- close buffer + hide terminal buffer
		["<leader>bk"] = {
			function()
				require("nvchad.tabufline").close_buffer()
			end,
			"Close buffer",
			opts = { silent = true },
		},
		["<S-Left>"] = { "<cmd>tabprevious<CR>", "Go to previous tab", opts = { silent = true } },
		["<S-Right>"] = { "<cmd>tabnext<CR>", "Go to next tab", opts = { silent = true } },
		["<S-Up>"] = { "<cmd>tabnew<CR>", "New tab", opts = { silent = true } },
		["<S-Down>"] = { "<cmd>tabclose<CR>", "Close tab", opts = { silent = true } },
	},
}

M.Git = {
	plugin = true,
	n = {
		["<leader>gg"] = {
			function()
				ToggleLazygit()
			end,
			"Lazygit",
			opts = { silent = true },
		},
		["<leader>gt"] = {
			function()
				require("telescope.builtin").git_status()
			end,
			"Git status",
			opts = { silent = true },
		},
		["<leader>gb"] = {
			function()
				require("telescope.builtin").git_branches()
			end,
			"Git branches",
			opts = { silent = true },
		},
		["<leader>gc"] = {
			function()
				require("telescope.builtin").git_commits()
			end,
			"Git commits",
			opts = { silent = true },
		},
		["<leader>gd"] = {
			function()
				if next(require("diffview.lib").views) == nil then
					vim.cmd("DiffviewOpen")
				else
					vim.cmd("DiffviewClose")
				end
			end,
			"Toggle Diffview",
			opts = { silent = true },
		},
	},
}

M.Comment = {
	-- toggle comment in both modes
	n = {
		["<leader>/"] = {
			function()
				require("Comment.api").toggle.linewise.current()
			end,
			"Toggle comment",
		},
	},

	v = {
		["<leader>/"] = {
			"<ESC><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>",
			"Toggle comment",
		},
	},
}

M.General = {
	i = {
		["jk"] = { "<ESC>", "escape Insert Mode", opts = { silent = true } },
		-- navigate within insert mode
		["<C-h>"] = { "<Left>", "Move left", opts = { silent = true } },
		["<C-l>"] = { "<Right>", "Move right", opts = { silent = true } },
		["<C-j>"] = { "<Down>", "Move down", opts = { silent = true } },
		["<C-k>"] = { "<Up>", "Move up", opts = { silent = true } },

		["<C-s>"] = { "<cmd> w <CR>", "Save file", opts = { silent = true } },
	},
	n = {
		-- switch between windows
		["<C-h>"] = { "<C-w>h", "Window left", opts = { silent = true } },
		["<C-l>"] = { "<C-w>l", "Window right", opts = { silent = true } },
		["<C-j>"] = { "<C-w>j", "Window down", opts = { silent = true } },
		["<C-k>"] = { "<C-w>k", "Window up", opts = { silent = true } },

		-- save
		["<C-s>"] = { "<cmd>w<CR>", "Save file", opts = { silent = true } },
		["<Esc>"] = { "<cmd>noh<CR>", "Clear highlights", opts = { silent = true } },
		["<leader>q"] = { "<cmd>q<cr>", "Quit", opts = { silent = true } },
		["<S-q>"] = { "<cmd>Bdelete<CR>", "Close Buffer", opts = { silent = true } },

		-- Resize with arrows
		["<C-Up>"] = { "<cmd>resize +2<CR>", "resize up", opts = { silent = true } },
		["<C-Down>"] = { "<cmd>resize -2<CR>", "resize down", opts = { silent = true } },
		["<C-Left>"] = { "<cmd>vertical resize +2<CR>", "resize left", opts = { silent = true } },
		["<C-Right>"] = { "<cmd>vertical resize -2<CR>", "resize right", opts = { silent = true } },
	},
	v = {
		["<C-s>"] = { "<cmd>w<CR>", "Save file" },
	},
	x = {
		["<C-s>"] = { "<cmd>w<CR>", "Save file" },
	},
	c = {
		["<Tab>"] = {
			function()
				if fn.getcmdtype() == "/" or fn.getcmdtype() == "?" then
					return "<CR>/<C-r>/"
				end
				return "<C-z>"
			end,
			"Word Search Increment",
			opts = { expr = true },
		},

		["<S-Tab>"] = {
			function()
				if fn.getcmdtype() == "/" or fn.getcmdtype() == "?" then
					return "<CR>?<C-r>/"
				end
				return "<S-Tab>"
			end,
			"Word Search Decrement",
			opts = { expr = true },
		},
	},
}

-- In order to disable a default keymap, use
M.disabled = {
	i = {
		["<C-b>"] = "",
		["<C-e>"] = "",
		["<C-h>"] = "",
		["<C-l>"] = "",
		["<C-j>"] = "",
		["<C-k>"] = "",
	},

	n = {
		-- cycle through buffers
		["<tab>"] = "",
		["<Up>"] = "",
		["<Down>"] = "",
		["<C-c>"] = "",
		["<C-s>"] = "",
		["<Esc>"] = "",
		["<leader>rn"] = "",
		["<leader>/"] = "",
		["<S-tab>"] = "",
		["<leader>x"] = "",
		["gD"] = "",
		["gd"] = "",
		["K"] = "",
		["gi"] = "",
		["<leader>ls"] = "",
		["<leader>lf"] = "",
		["<leader>D"] = "",
		["<leader>ra"] = "",
		["<leader>ca"] = "",
		["gr"] = "",
		["<leader>f"] = "",
		["[d"] = "",
		["]d"] = "",
		["<leader>q"] = "",
		["<leader>wa"] = "",
		["<leader>wr"] = "",
		["<leader>wl"] = "",
		["<leader>e"] = "",
		["<C-n>"] = "",
		["<leader>fa"] = "",
		["<leader>fw"] = "",
		["<leader>fb"] = "",
		["<leader>fh"] = "",
		["<leader>fo"] = "",
		["<leader>fz"] = "",
		["<leader>cm"] = "",
		["<leader>gt"] = "",
		["<leader>pt"] = "",
		["<leader>th"] = "",
		["<leader>ma"] = "",
		["<leader>wK"] = "",
		["<leader>wk"] = "",
		["<leader>cc"] = "",
		["]c"] = "",
		["[c"] = "",
		["<leader>rh"] = "",
		["<leader>ph"] = "",
		["<leader>gb"] = "",
		["<leader>td"] = "",
		["<A-i>"] = "",
		["<A-h>"] = "",
		["<A-v>"] = "",
		["<leader>h"] = "",
		["<leader>v"] = "",
		["<leader>ff"] = "",
		["<C-h>"] = "",
		["<C-l>"] = "",
		["<C-j>"] = "",
		["<C-k>"] = "",
		["<leader>n"] = "",
		["j"] = "",
		["k"] = "",
		["<leader>b"] = "",
		["<leader>ch"] = "",
		["<leader>fm"] = "",
	},
	t = {
		["<A-i>"] = "",
		["<A-h>"] = "",
		["<A-v>"] = "",
		["<C-x>"] = "",
	},
	v = {
		["<leader>/"] = "",
		["<Up>"] = "",
		["<Down>"] = "",
		["<"] = "",
		[">"] = "",
	},
	x = {
		["j"] = "",
		["k"] = "",
		["p"] = "",
	},
}

return M
