local status_ok, bufferline = pcall(require, "bufferline")
if not status_ok then
	return
end
bufferline.setup(astronvim.user_plugin_opts("plugins.bufferline", {
	options = {
		offsets = {
			{ filetype = "NvimTree", text = "", padding = 1 },
			{ filetype = "neo-tree", text = "", padding = 1 },
			{ filetype = "Outline", text = "", padding = 1 },
		},
		buffer_close_icon = "",
		modified_icon = "",
		close_icon = "",
		max_name_length = 14,
		max_prefix_length = 13,
		tab_size = 20,
		separator_style = "thick",
		diagnostics = "nvim_lsp",
		diagnostics_update_in_insert = true,
		diagnostics_indicator = function(diagnostics_dict)
			local s = " "
			for e, n in pairs(diagnostics_dict) do
				local sym = e == "error" and " " or (e == "warning" and " " or "")
				s = s .. n .. sym
			end
			return s
		end,
	},
}))
