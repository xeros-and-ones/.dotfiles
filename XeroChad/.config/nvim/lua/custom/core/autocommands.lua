-- ################
-- # Autocommands #
-- ################

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local fn = vim.fn
local general = augroup("General Settings", { clear = true })

-- local cmd = vim.api.nvim_create_user_command
-- local namespace = vim.api.nvim_create_namespace

autocmd("BufEnter", {
	callback = function()
		if #vim.api.nvim_list_wins() == 1 and vim.api.nvim_buf_get_name(0):match("NvimTree_") ~= nil then
			vim.cmd.quit()
		end
	end,
})
-- Hide folds and Disable statuscolumn in these filetypes
autocmd("FileType", {
	pattern = { "nvcheatsheet", "nvdash", "sagaoutline" },
	callback = function()
		vim.opt_local.foldcolumn = "0"
		vim.opt_local.stc = "" -- not really important
	end,
	group = general,
})

autocmd("FileType", {
	pattern = { "gitcommit", "markdown", "text", "log" },
	callback = function()
		vim.opt_local.wrap = true
		vim.opt_local.spell = true
	end,
	group = general,
	desc = "Enable Wrap in these filetypes",
})

autocmd({ "BufWinLeave" }, {
	pattern = { "*.*" },
	desc = "save view (folds), when closing file",
	command = "mkview",
})
autocmd({ "BufWinEnter" }, {
	pattern = { "*.*" },
	desc = "load view (folds), when opening file",
	command = "silent! loadview",
})

-- Stop newline comment continuation
autocmd("BufEnter", {
	callback = function()
		vim.opt.formatoptions:remove({ "c", "r", "o" })
	end,
	group = general,
	desc = "Disable New Line Comment",
})

-- open help in right split
local help_group = vim.api.nvim_create_augroup("help_window_right", { clear = true })
autocmd("BufWinEnter", {
	group = help_group,
	pattern = { "*.txt", "man", "*.md" },
	callback = function()
		if vim.o.filetype == "help" or "markdown" then
			vim.cmd.wincmd("L")
		end
	end,
	desc = "Open help pages in a vertical split",
})

-- highlight yanked text
autocmd("TextYankPost", {
	callback = function()
		require("vim.highlight").on_yank({ higroup = "CurSearch", timeout = 300 })
	end,
	group = general,
	desc = "Highlight when yanking",
})

-- resize splits if window got resized
autocmd("VimResized", {
	callback = function()
		vim.cmd("wincmd =")
	end,
	group = general,
	desc = "Equalize Splits",
})

-- close some filetypes with <q>
autocmd("FileType", {
	group = augroup("close_with_q", { clear = true }),
	pattern = {
		"PlenaryTestPopup",
		"help",
		"lspinfo",
		"man",
		"notify",
		"qf",
		"spectre_panel",
		"startuptime",
		"tsplayground",
	},
	callback = function(event)
		vim.bo[event.buf].buflisted = false
		vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
	end,
})

-- go to last loc when opening a buffer
autocmd("BufReadPost", {
	callback = function()
		if fn.line("'\"") > 1 and fn.line("'\"") <= fn.line("$") then
			vim.cmd('normal! g`"')
		end
	end,
	group = general,
	desc = "Go To The Last Cursor Position",
})

autocmd("BufWritePre", {
	group = augroup("auto_create_dir", { clear = true }),
	callback = function(event)
		local file = vim.loop.fs_realpath(event.match) or event.match

		vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
		local backup = vim.fn.fnamemodify(file, ":p:~:h")
		backup = backup:gsub("[/\\]", "%%")
		vim.go.backupext = backup
	end,
	desc = "create directories when needed, when saving a file",
})
