-- To find any highlight groups: "<cmd> Telescope highlights"
-- Each highlight group can take a table with variables fg, bg, bold, italic, etc
-- base30 variable names can also be used as colors

local M = {}

---@type Base46HLGroupsList
M.override = {
	Comment = {
		italic = true,
	},
	Search = { fg = "black2", bg = "yellow" },
	IncSearch = { fg = "black2", bg = "orange" },
	CurSearch = { link = "IncSearch" },
	Substitute = { fg = "black", bg = "green" },
	NvDashAscii = { bg = "NONE", fg = "blue" },
	NvDashButtons = { bg = "NONE" },
	FoldColumn = { bg = "NONE" },
	Underlined = { undercurl = true, sp = "blue" },
	CursorLine = { link = "ColorColumn" },
	CursorColumn = { link = "ColorColumn" },
	CursorLineNr = { link = "CursorColumn" },
	NeoTreeCursorColumn = { link = "ColorColumn" },
	NeoTreeDirectoryIcon = { link = "NvimTreeFolderIcon" },
	NeoTreeDirectoryName = { link = "NvimTreeOpenedFolderName" },
	NeoTreeSymbolicLinkTarget = { link = "NvimTreeSymlink" },
	NeoTreeRootName = { link = "NvimTreeRootFolder" },
	NeoTreeFileNameOpened = { link = "NvimTreeOpenedFile" },
	-- linked groups for all themes
	FlashPrompt = { link = "DiagnosticVirtualTextHint" },
	LspReferenceRead = { fg = "yellow", bg = "one_bg", bold = true },
	LspReferenceText = { fg = "yellow", bg = "one_bg", bold = true },
	LspReferenceWrite = { fg = "orange", bg = "one_bg", bold = true },
}

---@type HLTable
M.add = {
	NvimTreeOpenedFolderName = { fg = "green", bold = false },
	DiffviewDiffAdd = { fg = "black", bg = "green" },
	DiffviewDiffChange = { fg = "black", bg = "light_grey" },
	DiffviewDiffModified = { fg = "black", bg = "orange" },
	DiffviewDiffDelete = { fg = "black", bg = "red" },
	DiffviewDiffAddAsDelete = { fg = "black", bg = "red" },
	DiagnosticUnderlineWarn = { undercurl = true, sp = "yellow" },
	DiagnosticUnderlineOk = { undercurl = true, sp = "green" },
	DiagnosticUnderlineError = { undercurl = true, sp = "red" },
	DiagnosticUnderlineHint = { undercurl = true, sp = "light_grey" },
	DiagnosticUnderlineInfo = { undercurl = true, sp = "blue" },
}

return M
