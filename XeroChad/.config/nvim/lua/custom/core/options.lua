-- ########################
-- #   Neovim Options    #
-- #######################
local g = vim.g
local opt = vim.opt

g.mapleader = " " -- Set mapleader to space
g.snipmate_snippets_path = vim.fn.stdpath("config") .. "/lua/custom/snippets/snipmate"
g.vscode_snippets_path = vim.fn.stdpath("config") .. "/lua/custom/snippets/vscode"
g.lua_snippets_path = vim.fn.stdpath("config") .. "/lua/custom/snippets/luasnippets"
g.random_header = true -- Show random header
g.format_on_save = true
g.mkdp_auto_close = false
-------------------------------------- options ------------------------------------------
-- Override filetypes
vim.filetype.add({
	extension = {
		html = "htmldjango",
	},
})
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.textwidth = 88

opt.breakindent = true

vim.o.foldcolumn = "auto"
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99
vim.o.foldenable = true
vim.o.foldmethod = "indent"

opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = true
opt.infercase = true
opt.smartcase = true

opt.pumblend = 0 -- Make builtin completion menus slightly transparent
opt.pumheight = 10 -- Make popup menu smaller
opt.completeopt = { "menuone" } -- mostly just for cmp
-- opt.winblend = 0
opt.mousemoveevent = false -- screws with toggleterm input
opt.autochdir = false
opt.undofile = true
opt.undodir = vim.fn.stdpath("data") .. "/undo"

opt.verbose = 0
opt.report = 99999
opt.shortmess:append("sIAc")
opt.sessionoptions = "buffers,curdir,folds,tabpages,winsize,winpos,localoptions,globals,options"

opt.title = true
opt.titlelen = 0 -- do not shorten title
opt.titlestring = "NVIM => %t%( %M%)"

opt.hidden = true -- required to keep multiple buffers and open multiple buffers
opt.clipboard = "unnamedplus" -- allows neovim to access the system clipboard
opt.lazyredraw = false -- Won't be redrawn while executing macros, register and other commands.
opt.backup = false -- creates a backup file
opt.cmdheight = 1 -- more space in the neovim command line for displaying messages
opt.conceallevel = 0 -- so that `` is visible in markdown files
opt.fileencoding = "utf-8" -- the encoding written to a file
opt.mouse = "a" -- allow the mouse to be used in neovim
opt.showmode = false -- we don't need to see things like -- INSERT -- anymore
opt.smartindent = true -- make indenting smarter again
opt.splitbelow = true -- force all horizontal splits to go below current window
opt.splitright = true -- force all vertical splits to go to the right of current window
opt.swapfile = true -- creates a swapfile
opt.termguicolors = true -- set term gui colors (most terminals support this)
opt.timeout = true
opt.timeoutlen = 300 -- time to wait for a mapped sequence to complete (in milliseconds)
opt.updatetime = 250 -- faster completion (4000ms default)
opt.writebackup = false -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
opt.expandtab = true -- convert tabs to spaces
opt.cursorline = true -- highlight the current line
opt.number = true -- set numbered lines
opt.relativenumber = false
opt.laststatus = 3 -- only the last window will always have a status line
opt.showcmd = false -- hide (partial) command in the last line of the screen (for performance)
opt.ruler = false -- hide the line and column number of the cursor position
opt.numberwidth = 4 -- minimal number of columns to use for the line number {default 4}
opt.signcolumn = "yes" -- always show the sign column, otherwise it would shift the text each time
opt.wrap = false -- display lines as one long line
opt.scrolloff = 8 -- minimal number of screen lines to keep above and below the cursor
opt.sidescrolloff = 8 -- minimal number of screen columns to keep to the left and right of the cursor if wrap is `false`
opt.guifont = "monospace:h17" -- the font used in graphical neovim applications
opt.fillchars =
	{ diff = "╱", eob = " ", fold = " ", foldopen = "", foldsep = " ", foldclose = "", lastline = " " } -- make EndOfBuffer invisible
opt.whichwrap:append("<,>,[,],h,l")
opt.iskeyword:append("-") -- treats words with `-` as single words
opt.formatoptions:remove({ "c", "r", "o" }) -- This is a sequence of letters which describes how automatic formatting is to be done
opt.linebreak = true
