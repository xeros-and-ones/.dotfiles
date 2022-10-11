local cmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- go to last loc when opening a buffer
cmd("BufReadPost", { command = [[if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif]] })

-- set cursorColumn for python only on the current buffer
-- vim.cmd("autocmd FileType python set cursorcolumn")
local cursorColumn = augroup("CursorColumn", { clear = true })
cmd(
	{ "FileType", "BufEnter", "VimEnter", "WinEnter" },
	{ pattern = "*.py", command = "set cursorcolumn", group = cursorColumn }
)
cmd({ "WinLeave" }, { pattern = "*", command = "set nocursorcolumn", group = cursorColumn })

-- set cursorline on the current buffer only
local cursorGrp = augroup("CursorLine", { clear = true })
cmd({ "WinEnter" }, { pattern = "*", command = "set cursorline", group = cursorGrp })
cmd({ "WinLeave" }, { pattern = "*", command = "set nocursorline", group = cursorGrp })

vim.api.nvim_create_autocmd({ "FileType" }, {
	pattern = { "qf", "help", "man", "lspinfo", "spectre_panel" },
	callback = function()
		vim.cmd([[
      nnoremap <silent> <buffer> q :close<CR> 
      set nobuflisted 
    ]])
	end,
})

vim.api.nvim_create_autocmd({ "FileType" }, {
	pattern = { "gitcommit"},
	callback = function()
		vim.opt_local.wrap = true
		vim.opt_local.spell = true
	end,
})

vim.api.nvim_create_autocmd({ "FileType" }, {
	pattern = { "markdown" },
	callback = function()
		vim.opt_local.wrap = true
		vim.opt_local.spell = true
	end,
})


vim.cmd("autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif")
-- vim.api.nvim_create_autocmd({ "BufEnter" }, {
--   callback = function()
--     vim.cmd [[
--       if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif
--     ]]
--   end,
-- })

vim.api.nvim_create_autocmd({ "VimResized" }, {
	callback = function()
		vim.cmd("tabdo wincmd =")
	end,
})

vim.api.nvim_create_autocmd({ "CmdWinEnter" }, {
	callback = function()
		vim.cmd("quit")
	end,
})
vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
	callback = function()
		vim.cmd("set formatoptions-=cro")
	end,
})

vim.api.nvim_create_autocmd({ "TextYankPost" }, {
	callback = function()
		vim.highlight.on_yank({ clear = true })
	end,
})

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
	pattern = { "*.java" },
	callback = function()
		vim.lsp.codelens.refresh()
	end,
})

vim.api.nvim_create_autocmd({ "VimEnter" }, {
	callback = function()
		vim.cmd("hi link illuminatedWord LspReferenceText")
	end,
})
