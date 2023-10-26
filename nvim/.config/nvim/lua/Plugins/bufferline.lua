local M = {
    "akinsho/bufferline.nvim",
    version = "*",
    event = { "VimEnter" },
    dependencies = {
        {
            "famiu/bufdelete.nvim",
            commit = "8933abc09df6c381d47dc271b1ee5d266541448e",
        },
    },
}
function M.config()
    require("bufferline").setup {
        options = {
            mode = "buffers",                    -- set to "tabs" to only show tabpages instead
            numbers = "none",
            left_mouse_command = "buffer %d",    -- can be a string | function, see "Mouse actions"
            close_command = "Bdelete! %d",       -- can be a string | function, see "Mouse actions"
            right_mouse_command = "Bdelete! %d", -- can be a string | function, see "Mouse actions"
            indicator = {
                icon = "▎", -- this should be omitted if indicator style is not 'icon'
                style = 'icon',
            },
            buffer_close_icon = "",
            modified_icon = "●",
            close_icon = "",
            left_trunc_marker = "",
            right_trunc_marker = "",
            max_name_length = 22,
            max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
            truncate_names = true,  -- whether or not tab names should be truncated
            tab_size = 22,
            diagnostics = "nvim_lsp",
            diagnostics_update_in_insert = true,
            diagnostics_indicator = function(count, level, diagnostics_dict, context)
                local s = " "
                for e, n in pairs(diagnostics_dict) do
                    local sym = e == "error" and "  " or (e == "warning" and "  " or "  ")
                    s = s .. n .. sym
                end
                return s
            end,
            -- diagnostics_indicator = function(count, level, diagnostics_dict, context)
            -- 	local icon = level:match("error") and " " or " "
            -- 	return " " .. icon .. count
            -- end,
            color_icons = true,       -- whether or not to add the filetype icon highlights
            show_buffer_icons = true, -- disable filetype icons for buffers
            show_buffer_close_icons = true,
            show_close_icon = true,
            show_tab_indicators = true,
            show_duplicate_prefix = true, -- whether to show duplicate buffer prefix
            persist_buffer_sort = true,   -- whether or not custom sorted buffers should persist
            separator_style = "thin",
            enforce_regular_tabs = false,
            always_show_bufferline = true,
            sort_by = "directory",
            offsets = {
                {
                    filetype = "aerial",
                    text = "Outline",
                    text_align = "center",
                    separator = false,
                },
                {
                    filetype = "OverseerList",
                    text = "Tasks",
                    text_align = "center",
                    separator = false,
                },
                {
                    filetype = "NvimTree",
                    text = "FILE EXPLORER",
                    text_align = "center",
                    separator = true,
                },
                {
                    filetype = "neo-tree",
                    text = "FILE EXPLORER",
                    highlight = "directory",
                    text_align = "center",
                    separator = true,
                },
            },
        },
        highlights = {
            buffer_selected = {
                fg = "#8cbe7a",
                bold = true,
                bg = "#000000"
            },
            close_button_selected = {
                fg = "#f35858",
                bg = "#000000"
            },
            diagnostic_selected = {
                bg = '#000000',
                bold = true,
            },
            tab_selected = {
                bg = '#000000',
            },
            numbers_selected = {
                bg = '#000000',
                bold = true,
            },
            hint_selected = {
                bg = '#000000',
                bold = true,
            },
            hint_diagnostic_selected = {
                bg = '#000000',
                bold = true,
            },
            info_diagnostic_selected = {
                bg = '#000000',
                bold = true,
            },
            info_selected = {
                bg = '#000000',
                bold = true,
            },
            warning_diagnostic_selected = {
                bg = '#000000',
                bold = true,
            },
            warning_selected = {
                bg = '#000000',
                bold = true,
            },
            error_diagnostic_selected = {
                bg = '#000000',
                bold = true,
            },
            error_selected = {
                bg = '#000000',
                bold = true,
            },
            modified_selected = {
                bg = '#000000',
            },
        },
    }
end

return M
