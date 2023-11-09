---@type ChadrcConfig
local M = {}

local headers = require("custom.core.headers")

local function get_header()
	if vim.g.random_header then
		local headerNames = {}
		for name, _ in pairs(headers) do
			table.insert(headerNames, name)
		end

		local randomName = headerNames[math.random(#headerNames)]
		local randomHeader = headers[randomName]
		return randomHeader
	else
		return headers["nvchad"]
	end
end

local highlights = require("custom.highlights")

M.ui = {
	theme = "gruvchad",
	transparency = true,
	theme_toggle = { "gruvchad", "one_light" },
	lsp_semantic_tokens = true, -- needs nvim v0.9, just adds highlight groups for lsp semantic tokens
	hl_override = highlights.override,
	hl_add = highlights.add,
	extended_integrations = {
		"notify",
		"dap",
		"lspsaga",
		"todo",
		"trouble",
		"navic",
	}, -- these aren't compiled by default, ex: "alpha", "notify"
	telescope = { style = "bordered" }, -- borderless / bordered
	cmp = {
		icons = true,
		lspkind_text = true,
		style = "default", -- default/flat_light/flat_dark/atom/atom_colored
		border_color = "grey_fg", -- only applicable for "default" style, use color names from base30 variables
		selected_item_bg = "colored", -- colored / simple
	},
	statusline = {
		theme = "minimal", -- default/vscode/vscode_colored/minimal
		-- default/round/block/arrow separators work only for default statusline theme
		-- round and block will work for minimal theme only
		separator_style = "round",
		overriden_modules = function(modules)
			local config = require("core.utils").load_config().ui.statusline
			local sep_style = config.separator_style

			sep_style = (sep_style ~= "round" and sep_style ~= "block") and "block" or sep_style

			local default_sep_icons = {
				round = { left = "", right = "" },
				block = { left = "█", right = "█" },
			}

			local fn = vim.fn
			local separators = (type(sep_style) == "table" and sep_style) or default_sep_icons[sep_style]

			local sep_l = separators["left"]
			local sep_r = "%#St_sep_r#" .. separators["right"] .. " %#ST_EmptySpace#"

			local function stbufnr()
				return vim.api.nvim_win_get_buf(vim.g.statusline_winid)
			end
			local function gen_block(icon, txt, sep_l_hlgroup, iconHl_group, txt_hl_group)
				return sep_l_hlgroup .. sep_l .. iconHl_group .. icon .. " " .. txt_hl_group .. " " .. txt .. sep_r
			end
			local function is_activewin()
				return vim.api.nvim_get_current_win() == vim.g.statusline_winid
			end
			local noice_ok, noice = pcall(require, "noice.api")
			local modes = {
				["n"] = { "NORMAL", "St_NormalMode" },
				["no"] = { "NORMAL (no)", "St_NormalMode" },
				["nov"] = { "NORMAL (nov)", "St_NormalMode" },
				["noV"] = { "NORMAL (noV)", "St_NormalMode" },
				["noCTRL-V"] = { "NORMAL", "St_NormalMode" },
				["niI"] = { "NORMAL i", "St_NormalMode" },
				["niR"] = { "NORMAL r", "St_NormalMode" },
				["niV"] = { "NORMAL v", "St_NormalMode" },
				["nt"] = { "NTERMINAL", "St_NTerminalMode" },
				["ntT"] = { "NTERMINAL (ntT)", "St_NTerminalMode" },

				["v"] = { "VISUAL", "St_VisualMode" },
				["vs"] = { "V-CHAR (Ctrl O)", "St_VisualMode" },
				["V"] = { "V-LINE", "St_VisualMode" },
				["Vs"] = { "V-LINE", "St_VisualMode" },
				[""] = { "V-BLOCK", "St_VisualMode" },

				["i"] = { "INSERT", "St_InsertMode" },
				["ic"] = { "INSERT (completion)", "St_InsertMode" },
				["ix"] = { "INSERT completion", "St_InsertMode" },

				["t"] = { "TERMINAL", "St_TerminalMode" },

				["R"] = { "REPLACE", "St_ReplaceMode" },
				["Rc"] = { "REPLACE (Rc)", "St_ReplaceMode" },
				["Rx"] = { "REPLACEa (Rx)", "St_ReplaceMode" },
				["Rv"] = { "V-REPLACE", "St_ReplaceMode" },
				["Rvc"] = { "V-REPLACE (Rvc)", "St_ReplaceMode" },
				["Rvx"] = { "V-REPLACE (Rvx)", "St_ReplaceMode" },

				["s"] = { "SELECT", "St_SelectMode" },
				["S"] = { "S-LINE", "St_SelectMode" },
				[""] = { "S-BLOCK", "St_SelectMode" },
				["c"] = { "COMMAND", "St_CommandMode" },
				["cv"] = { "COMMAND", "St_CommandMode" },
				["ce"] = { "COMMAND", "St_CommandMode" },
				["r"] = { "PROMPT", "St_ConfirmMode" },
				["rm"] = { "MORE", "St_ConfirmMode" },
				["r?"] = { "CONFIRM", "St_ConfirmMode" },
				["x"] = { "CONFIRM", "St_ConfirmMode" },
				["!"] = { "SHELL", "St_TerminalMode" },
			}

			modules[1] = (function()
				if not is_activewin() then
					return ""
				end
				local m = vim.api.nvim_get_mode().mode
				if require("hydra.statusline").is_active() then
					return gen_block(
						"󱔎",
						require("hydra.statusline").get_name(),
						"%#St_InsertModeSep#",
						"%#St_InsertMode#",
						"%#St_InsertModeText#"
					)
				else
					return gen_block(
						"",
						modes[m][1],
						"%#" .. modes[m][2] .. "Sep#",
						"%#" .. modes[m][2] .. "#",
						"%#" .. modes[m][2] .. "Text#"
					)
				end
			end)()

			modules[3] = (function()
				if not vim.b[stbufnr()].gitsigns_head or vim.b[stbufnr()].gitsigns_git_status then
					return ""
				end

				local git_status = vim.b[stbufnr()].gitsigns_status_dict

				local added = (git_status.added and git_status.added ~= 0) and ("  " .. git_status.added) or ""
				local changed = (git_status.changed and git_status.changed ~= 0) and ("  " .. git_status.changed)
					or ""
				local removed = (git_status.removed and git_status.removed ~= 0) and ("  " .. git_status.removed)
					or ""
				local branch_name = git_status.head
				local rg = {
					" ⟶",
					"%#St_lsp_txt#" .. added,
                    "%#St_Pos_txt#" .. changed,
					"%#St_file_txt#" .. removed,
				}
				return (
					vim.o.columns > 100
						and gen_block(
							"",
							branch_name .. table.concat(rg, ""),
							"%#St_lsp_sep#",
							"%#St_lsp_bg#",
							"%#St_lsp_txt#"
						)
					or "%#DiffAdded#" .. added .. "%#DiffModified#" .. changed .. "%#DiffRemoved#" .. removed
				)
			end)()

			modules[6] = (function()
				if noice_ok and noice.status.mode.has() then
					return "%#St_lsp_sep#" .. noice.status.mode.get() .. " "
				else
					return " "
				end
			end)()
			modules[7] = (function()
				if noice_ok and noice.status.command.has() then
					return "%#NoTexthl#" .. noice.status.command.get() .. " "
				else
					return " "
				end
			end)()
			modules[9] = (function()
				local clients = {}
				local buf = vim.api.nvim_get_current_buf()

				-- Iterate through all the clients for the current buffer
				for _, client in pairs(vim.lsp.get_active_clients({ bufnr = buf })) do
					-- Add the client name to the `clients` table
					if client.name ~= "null-ls" then
						table.insert(clients, 1, client.name)
					elseif client.name == "null-ls" then
						for _, source in pairs(require("null-ls.sources").get_available(vim.bo.filetype)) do
							table.insert(clients, source.name)
						end
					end
				end

				if #clients == 0 then
					return ""
				else
					return (
						vim.o.columns > 130
						and gen_block(
							" ",
							table.concat(clients, ", "),
							"%#St_lsp_sep#",
							"%#St_lsp_bg#",
							"%#St_lsp_txt#"
						)
					) or gen_block("", clients[1], "%#St_lsp_sep#", "%#St_lsp_bg#", "%#St_lsp_txt#")
				end
			end)()
			modules[10] = (function()
				return (
					vim.o.columns > 140
					and gen_block(
						"",
						fn.fnamemodify(fn.getcwd(), ":r:t"),
						"%#St_cwd_sep#",
						"%#St_cwd_bg#",
						"%#St_cwd_txt#"
					)
				)
					or gen_block(
						"",
						fn.fnamemodify(fn.getcwd(), ":t"),
						"%#St_cwd_sep#",
						"%#St_cwd_bg#",
						"%#St_cwd_txt#"
					)
			end)()
			modules[11] = (function()
				return (
					vim.o.columns > 120
						and gen_block("", "%p%% ┃ %l:%L", "%#St_Pos_sep#", "%#St_Pos_bg#", "%#St_Pos_txt#")
					or gen_block("", "%l", "%#St_Pos_sep#", "%#St_Pos_bg#", "%#St_Pos_txt#")
				)
			end)()
		end,
	},

	-- lazyload it when there are 1+ buffers
	tabufline = {
		show_numbers = false,
		enabled = true,
		lazyload = true,
	},

	nvdash = {
		load_on_startup = true,
		header = get_header(),
		buttons = {
			{ "🗐  " .. " Find File", "Spc f f", "Telescope find_files" },
			{ "  " .. " Recent Files", "Spc f r", "Telescope oldfiles" },
			{ "  " .. " Live Grep", "Spc f w", "Telescope live_grep" },
			{ "  " .. " Restore Session", "Spc o s", "lua require('persistence').load({ last = true })" },
			{ "  " .. " Find Projects", "Spc f p", "Telescope projects" },
			{ "  " .. " Themes", "Spc f t", "Telescope themes" },
			{ "  " .. " Mappings", "Spc n c", "NvCheatsheet" },
		},
	},

	cheatsheet = { theme = "grid" }, -- simple/grid
	lsp = {
		signature = {
			disabled = true,
		},
	},
}

M.lazy_nvim = require("custom.core.lazy") -- config for lazy.nvim startup options

M.plugins = "custom.plugins"

-- check core.mappings for table structure
M.mappings = require("custom.mappings")

return M
