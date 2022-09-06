local impatient_ok, impatient = pcall(require, "impatient")
if impatient_ok then
	impatient.enable_profile()
end

for _, source in ipairs({
	"core.utils",
	"core.options",
	"core.bootstrap",
	"core.plugins",
	"core.autocmds",
	"core.mappings",
	"configs.which-key-register",
}) do
	local status_ok, fault = pcall(require, source)
	if not status_ok then
		vim.api.nvim_err_writeln("Failed to load " .. source .. "\n\n" .. fault)
	end
end

astronvim.conditional_func(astronvim.user_plugin_opts("polish", nil, false))

-- tokyonight config
-- vim.g.tokyonight_style = "night"
-- vim.g.tokyonight_terminal_colors = true
-- vim.g.tokyonight_italic_functions = true
-- vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer", "neo-tree" }
-- vim.g.tokyonight_transparent_sidebar = true
-- vim.g.tokyonight_dark_float = true
-- vim.g.tokyonight_lualine_bold = true
-- -- Change the "hint" color to the "orange" color, and make the "error" color bright red
-- vim.g.tokyonight_colors = { hint = "orange", error = "#fc443a" }
-- -- Load the colorscheme
-- vim.cmd([[colorscheme tokyonight]])

--gruvbox-baby config
vim.g.gruvbox_baby_keyword_style = "italic"
vim.g.gruvbox_use_original_palette = true
-- Enable transparent mode
vim.g.gruvbox_baby_transparent_mode = 1

-- Load the colorscheme
vim.cmd([[colorscheme gruvbox-baby]])
