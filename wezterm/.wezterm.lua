-- Pull in the wezterm API
local wezterm = require("wezterm")
local act = wezterm.action

-- This table will hold the configuration.
local config = {}
config.disable_default_key_bindings = true

-- CORE
-- config.enable_wayland = false
config.show_new_tab_button_in_tab_bar = false
config.warn_about_missing_glyphs = false
-- config.enable_kitty_keyboard = true
config.mouse_wheel_scrolls_tabs = false
config.underline_position = -4
config.initial_cols = 165
config.initial_rows = 40
config.scrollback_lines = 5000
config.inactive_pane_hsb = {
	saturation = 0.9,
	brightness = 0.4,
}

-- GUI
config.adjust_window_size_when_changing_font_size = false
config.color_scheme = "GruvboxDark"
config.colors = {
	background = "#222222",
	tab_bar = {
		background = "#000000",
		-- The color of the strip that goes along the top of the window
		-- (does not apply when fancy tab bar is in use)

		-- The active tab is the one that has focus in the window
		active_tab = {
			-- The color of the background area for the tab
			bg_color = "#8fd479",
			-- The color of the text for the tab
			fg_color = "#000000",

			-- "Half", "Normal" or "Bold"
			intensity = "Bold",
		},

		-- Inactive tabs are the tabs that do not have focus
		inactive_tab = {
			bg_color = "#000000",
			fg_color = "#adadad",
			intensity = "Half",
			-- The same options that were listed under the `active_tab` section above
			-- can also be used for `inactive_tab`.
		},
	},
}

function tab_title(tab_info)
	local title = tab_info.tab_title
	-- if the tab title is explicitly set, take that
	if title and #title > 0 then
		return title
	end
	-- Otherwise, use the title from the active pane
	-- in that tab
	return tab_info.active_pane.title
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	local title = tab_title(tab)
	return " " .. title .. " "
end)
config.window_background_opacity = 0.80
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.tab_max_width = 60
config.show_tab_index_in_tab_bar = false
config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

-- COMMAND PALETTE
config.command_palette_font_size = 12

-- CURSOR
config.cursor_blink_rate = 0

-- FONTS
config.font_size = 10
-- config.bold_brightens_ansi_colors = true
config.font = wezterm.font("FiraCode Nerd Font", {
	style = "Normal",
	weight = "DemiBold",
})
-- config.font_rules = {
-- 	{
-- 		italic = true,
-- 		intensity = "Bold",
-- 		font = wezterm.font("CaskaydiaCove Nerd Font", { weight = "Bold", style = "Italic" }),
-- 	},
-- 	{
-- 		italic = true,
-- 		intensity = "Half",
-- 		font = wezterm.font("CaskaydiaCove Nerd Font", { style = "Italic" }),
-- 	},
-- 	{
-- 		italic = true,
-- 		intensity = "Normal",
-- 		font = wezterm.font("CaskaydiaCove Nerd Font", { weight = "DemiBold", style = "Italic" }),
-- 	},
-- }

-- KEY_BINDING
config.swap_backspace_and_delete = false
config.keys = {
	{ key = "Enter", mods = "ALT", action = act.ToggleFullScreen },
	{ key = "T", mods = "SHIFT|CTRL", action = act.SpawnTab("DefaultDomain") },
	{ key = "{", mods = "SHIFT|CTRL", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "}", mods = "SHIFT|CTRL", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "Enter", mods = "SHIFT|CTRL", action = act.RotatePanes("Clockwise") },
	{ key = "Tab", mods = "CTRL", action = act.ActivateTabRelative(1) },
	{ key = "Tab", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(-1) },
	{ key = "Backspace", mods = "SHIFT|CTRL", action = act.ResetFontSize },
	{ key = "+", mods = "SHIFT|CTRL", action = act.IncreaseFontSize },
	{ key = "_", mods = "SHIFT|CTRL", action = act.DecreaseFontSize },
	{ key = "V", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },
	{ key = "Insert", mods = "SHIFT", action = act.PasteFrom("PrimarySelection") },
	{ key = "C", mods = "SHIFT|CTRL", action = act.CopyTo("Clipboard") },
	{ key = "Insert", mods = "CTRL", action = act.CopyTo("PrimarySelection") },
	{ key = "F", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },
	{ key = "K", mods = "SHIFT|CTRL", action = act.ClearScrollback("ScrollbackOnly") },
	{ key = "L", mods = "SHIFT|CTRL", action = act.ShowDebugOverlay },
	{ key = "N", mods = "SHIFT|CTRL", action = act.SpawnWindow },
	{ key = "P", mods = "SHIFT|CTRL", action = act.ActivateCommandPalette },
	{ key = "R", mods = "SHIFT|CTRL", action = act.ReloadConfiguration },
	{ key = "T", mods = "SHIFT|CTRL|ALT", action = act.ShowTabNavigator },
	{ key = "S", mods = "SHIFT|CTRL", action = act.PaneSelect },
	{
		key = "U",
		mods = "SHIFT|CTRL",
		action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
	},
	{ key = "W", mods = "SHIFT|CTRL", action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "W", mods = "SHIFT|CTRL", action = act.CloseCurrentPane({ confirm = true }) },
	{ key = "X", mods = "SHIFT|CTRL", action = act.ActivateCopyMode },
	{ key = "Z", mods = "SHIFT|CTRL", action = act.TogglePaneZoomState },
	{ key = "phys:Space", mods = "SHIFT|CTRL", action = act.QuickSelect },
	{ key = "PageUp", mods = "SHIFT", action = act.ScrollByPage(-0.5) },
	{ key = "PageDown", mods = "SHIFT", action = act.ScrollByPage(0.5) },
	{ key = "PageUp", mods = "SHIFT|CTRL", action = act.MoveTabRelative(-1) },
	{ key = "PageDown", mods = "SHIFT|CTRL", action = act.MoveTabRelative(1) },
	{ key = "LeftArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Left") },
	{ key = "RightArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Right") },
	{ key = "UpArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Up") },
	{ key = "DownArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Down") },
	{ key = "H", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Left") },
	{ key = "L", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Right") },
	{ key = "K", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Up") },
	{ key = "J", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Down") },
	{ key = "LeftArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Left", 3 }) },
	{ key = "RightArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Right", 3 }) },
	{ key = "UpArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Up", 3 }) },
	{ key = "DownArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Down", 3 }) },
	{ key = "H", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Left", 3 }) },
	{ key = "L", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Right", 3 }) },
	{ key = "K", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Up", 3 }) },
	{ key = "J", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Down", 3 }) },
}

-- and finally, return the configuration to wezterm
return config
