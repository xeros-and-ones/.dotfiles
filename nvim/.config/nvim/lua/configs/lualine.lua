local status_ok = pcall(require, "lualine")
if not status_ok then
	return
end

local hide_in_width = function()
	return vim.fn.winwidth(0) > 80
end

local diagnostics = {
	"diagnostics",
	sources = { "nvim_diagnostic" },
	sections = { "error", "warn", "info", "hint" },
	symbols = { error = " ", warn = " ", info = " ", hint = "" },
	colored = true,
	always_visible = false,
	update_in_insert = true,
}

local diff = {
	"diff",
	colored = true,
	symbols = { added = " ", modified = " ", removed = " " }, -- changes diff symbols
	cond = hide_in_width,
}

local mode = {
	"mode",
	fmt = function(str)
		return str
	end,
}

local filetype = {
	"filetype",
	icons_enabled = false,
	icon = nil,
	cond = hide_in_width,
}

local branch = {
	"branch",
	icons_enabled = true,
	icon = "",
}

local location = {
	"location",
	padding = 0,
	cond = hide_in_width,
}

-- cool function for progress
local progress = {
	cond = hide_in_width,
	function()
		local current_line = vim.fn.line(".")
		local total_lines = vim.fn.line("$")
		local chars = { "__", "▁▁", "▂▂", "▃▃", "▄▄", "▅▅", "▆▆", "▇▇", "██" }
		local line_ratio = current_line / total_lines
		local index = math.ceil(line_ratio * #chars)
		return chars[index]
	end,
}

local lsp = {
	-- Lsp server name .
	function()
		local msg = "No Active Lsp"
		local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
		local clients = vim.lsp.get_active_clients()
		if next(clients) == nil then
			return msg
		end
		for _, client in ipairs(clients) do
			local filetypes = client.config.filetypes
			if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
				return client.name
			end
		end
		return msg
	end,
	icon = " ",
	color = { gui = "bold" },
}
local treesitter = {
	cond = hide_in_width,
	function()
		local ts_avail, ts = pcall(require, "nvim-treesitter.parsers")
		return (ts_avail and ts.has_parser()) and " 綠TS" or "ﳠ No TS"
	end,
}
--local spaces = function()
--return "spaces: " .. vim.api.nvim_buf_get_option(0, "shiftwidth")
--end

local format = {
	"fileformat",
	cond = hide_in_width,
}
local encoding = {
	"encoding",
	cond = hide_in_width,
}

require("lualine").setup({
	options = {
		icons_enabled = true,
		theme = "powerline_dark",
		component_separators = { left = "", right = "" },
		section_separators = { left = "", right = "" },
		disabled_filetypes = {
			"alpha",
			"neo-tree",
			"dashboard",
			"Outline",
			statusline = {},
			winbar = {},
		},
		ignore_focus = {},
		always_divide_middle = true,
		globalstatus = false,
		refresh = {
			statusline = 1000,
			tabline = 1000,
			winbar = 1000,
		},
	},

	sections = {
		lualine_a = { mode },
		lualine_b = { branch, diff, diagnostics },
		lualine_c = {},
		lualine_x = { format, encoding, treesitter },
		lualine_y = { lsp, filetype },
		lualine_z = { "filename", location, progress },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = { "filename" },
		lualine_x = { "location" },
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {},
	winbar = {},
	inactive_winbar = {},
	extensions = {},
})
