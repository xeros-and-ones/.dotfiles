local status_ok, bufferline = pcall(require, "bufferline")
if not status_ok then
	return
end
bufferline.setup(astronvim.user_plugin_opts("plugins.bufferline", {
	options = {
		offsets = {
			{ filetype = "NvimTree", text = "FILE EXPLORER", text_align = "center", separator = true },
			{ filetype = "neo-tree", text = "FILE EXPLORER", text_align = "center", separator = true },
			{ filetype = "Outline", text = "FILE EXPLORER", text_align = "center", separator = true },
		},
		buffer_close_icon = "",
		modified_icon = "",
		close_icon = "",
		max_name_length = 14,

		max_prefix_length = 13,
		tab_size = 20,
		show_tab_indicatior = true,
		separator_style = "thick",
		diagnostics = "nvim_lsp",
		diagnostics_update_in_insert = true,
		always_show_bufferline = true,
		diagnostics_indicator = function(count, level, diagnostics_dict, context)
			local s = " "
			for e, n in pairs(diagnostics_dict) do
				local sym = e == "error" and "  " or (e == "warning" and "  " or " ")
				s = s .. n .. sym
			end
			return s
		end,
		-- diagnostics_indicator = function(count, level, diagnostics_dict, context)
		-- 	local icon = level:match("error") and " " or " "
		-- 	return " " .. icon .. count
		-- end,
	},
}))
