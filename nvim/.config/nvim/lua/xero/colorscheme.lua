local colorscheme = "gruvbox"
require("gruvbox").setup({
  undercurl = true,
  bold = true,
  italic = false,
  strikethrough = true,
  invert_selection = false,
  invert_signs = false,
  invert_tabline = false,
  invert_intend_guides = false,
  inverse = true, -- invert background for search, diffs, statuslines and errors
  contrast = "hard", -- can be "hard", "soft" or empty string
  transparent_mode = true,
  overrides = {
    DiagnosticVirtualTextError = { fg = "#fb4934", bg = "#400404" },
    DiagnosticVirtualTextWarn = { fg = "#fabd2f", bg = "#3f4004" },
    DiagnosticVirtualTextInfo = { fg = "#83a598", bg = "#040540" },
    DiagnosticVirtualTextHint = { fg = "#427b58", bg = "#043d40" },
    BufferLineBufferSelected = { fg = "#637CF7", bold = true },
  },
})
local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
  return
end
