-- ################
-- # Autocommands #
-- ################

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local fn = vim.fn
local general = augroup("General Settings", { clear = true })

-- local cmd = vim.api.nvim_create_user_command
-- local namespace = vim.api.nvim_create_namespace

autocmd("FileType", {
	pattern = "*.py",
	callback = function()
		vim.opt_local.cursorColumn = true
	end,
	desc = "set cursorColumn for python only on the current buffer",
})

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

autocmd("TermOpen", {
	callback = function()
		vim.opt_local.relativenumber = false
		vim.opt_local.number = false
		vim.cmd("startinsert!")
	end,
	group = general,
	desc = "Terminal Options",
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

autocmd({ "BufWinEnter" }, {
	callback = function()
		local line_count = vim.api.nvim_buf_line_count(0)
		if line_count >= 5000 then
			vim.cmd("IlluminatePauseBuf")
		end
	end,
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

-- show cursor line only in active window
autocmd({ "InsertLeave", "WinEnter" }, {
	callback = function()
		local ok, cl = pcall(vim.api.nvim_win_get_var, 0, "auto-cursorline")
		if ok and cl then
			vim.wo.cursorline = true
			vim.api.nvim_win_del_var(0, "auto-cursorline")
		end
	end,
})
autocmd({ "InsertEnter", "WinLeave" }, {
	callback = function()
		local cl = vim.wo.cursorline
		if cl then
			vim.api.nvim_win_set_var(0, "auto-cursorline", cl)
			vim.wo.cursorline = false
		end
	end,
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

-- autocmd("BufEnter", {
-- 	desc = "Open Neo-Tree on startup with directory",
-- 	group = augroup("neotree_start", { clear = true }),
-- 	callback = function()
-- 		local stats = vim.loop.fs_stat(vim.api.nvim_buf_get_name(0))
-- 		if stats and stats.type == "directory" then
-- 			require("neo-tree.setup.netrw").hijack()
-- 		end
-- 	end,
-- })
autocmd("VimEnter", {
	callback = function(data)
		-- buffer is a directory
		local directory = vim.fn.isdirectory(data.file) == 1

		-- change to the directory
		if directory then
			vim.cmd.cd(data.file)
			-- open the tree
			require("nvim-tree.api").tree.open()
		end
	end,
	group = general,
	desc = "Open NvimTree when it's a Directory",
})
