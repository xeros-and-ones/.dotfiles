local M = {
	"lukas-reineke/indent-blankline.nvim",
	main = "ibl",
	enabled = true,
	event = { "BufReadPre", "BufNewFile" },
}

function M.init()
	local present, blank = pcall(require, "ibl")
	if not present then
		return
	end
	blank.setup({
		indent = { char = "▏", smart_indent_cap = true },
		scope = {
			enabled = true,
			show_start = true,
			show_end = false,
			injected_languages = true,
		},
		exclude = {
			filetypes = {
				"help",
				"terminal",
				"dashboard",
				"alpha",
				"lazy",
				"NvimTree",
				"packer",
				"noice",
				"notify",
				"lspinfo",
				"TelescopePrompt",
				"OverseerForm",
				"TelescopeResults",
				"lsp-installer",
				"",
			},
			buftypes = { "terminal", "dashboard", "notify" },
		},
	})
end

return M
