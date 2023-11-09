return {
	single_file_support = true,
	settings = {
		root_dir = function()
			return vim.loop.cwd()
		end,
	},
}
