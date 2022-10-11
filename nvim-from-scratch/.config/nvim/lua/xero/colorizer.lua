local status_ok, colorizer = pcall(require, "colorizer")
if not status_ok then
	return
end
require('colorizer').setup = {
	RGB = true, -- #RGB hex codes
	RRGGBB = true, -- #RRGGBB hex codes
	names = false, -- "Name" codes like Blue
	RRGGBBAA = true, -- #RRGGBBAA hex codes
	rgb_fn = false, -- CSS rgb() and rgba() functions
	hsl_fn = false, -- CSS hsl() and hsla() functions
	css = true, -- Enable all css features: rgb_fn, hsl_fn, names, RGB, RRGGBB
	css_fn = false, -- Enable all CSS *functions*: rgb_fn, hsl_fn
	mode = "background", -- Set the display mode
}

