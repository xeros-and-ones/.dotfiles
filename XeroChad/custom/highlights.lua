-- To find any highlight groups: "<cmd> Telescope highlights"
-- Each highlight group can take a table with variables fg, bg, bold, italic, etc
-- base30 variable names can also be used as colors

local M = {}

---@type Base46HLGroupsList
M.override = {
	Comment = {
		italic = true,
	},
	Search = { fg = "black", bg = "blue" },
	IncSearch = { fg = "black", bg = "red" },
	CurSearch = { fg = "black", bg = "blue" },
	Substitute = { fg = "black", bg = "green" },
	NvDashAscii = { bg = "NONE", fg = "blue" },
	NvDashButtons = { bg = "NONE" },
	FoldColumn = { bg = "NONE" },
	LspReferenceRead = { link = "Underlined" },
	LspReferenceText = { link = "Underlined" },
	LspReferenceWrite = { link = "Underlined" },
	Underlined = { undercurl = true, sp = "white" },
	CursorLine = { link = "ColorColumn" },
	CursorColumn = { link = "ColorColumn" },
	NeoTreeCursorColumn = { link = "ColorColumn" },
	NeoTreeDirectoryIcon = { link = "NvimTreeFolderIcon" },
	NeoTreeDirectoryName = { link = "NvimTreeOpenedFolderName" },
	NeoTreeSymbolicLinkTarget = { link = "NvimTreeSymlink" },
	NeoTreeRootName = { link = "NvimTreeRootFolder" },
	NeoTreeFileNameOpened = { link = "NvimTreeOpenedFile" },
	-- linked groups for all themes
	TroubleCount = { link = "DiagnosticOk" },
	TroubleTextHint = { link = "DiagnosticHint" },
	TroubleTextError = { link = "DiagnosticError" },
	TroubleTextWarning = { link = "DiagnosticWarn" },
	TroubleTextInformation = { link = "DiagnosticInfo" },
	FlashPrompt = { link = "DiagnosticVirtualTextHint" },
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
	Underlined = { undercurl = true, sp = "white" },
}

return M
