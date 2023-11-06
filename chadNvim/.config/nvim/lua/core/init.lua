local opt = vim.opt
local g = vim.g
local config = require("core.utils").load_config()

-------------------------------------- globals -----------------------------------------
g.nvchad_theme = config.ui.theme
g.base46_cache = vim.fn.stdpath "data" .. "/nvchad/base46/"
g.toggle_theme_icon = "   "
g.transparency = config.ui.transparency

-- disable some default providers
for _, provider in ipairs { "node", "perl", "python3", "ruby" } do
  vim.g["loaded_" .. provider .. "_provider"] = 0
end

g.mkdp_auto_close = false -- Don't Exit Preview When Switching Buffers
g.mapleader = " " -- Set mapleader to space
g.snipmate_snippets_path = vim.fn.stdpath "config" .. "/lua/custom/snippets/snipmate"
g.random_header = false -- Show random header
-------------------------------------- options ------------------------------------------
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
opt.completeopt = { "menuone", "noselect" } -- mostly just for cmp
-- opt.winblend = 0
opt.mousemoveevent = false -- screws with toggleterm input

opt.autochdir = false
opt.undofile = true
opt.undodir = vim.fn.stdpath "data" .. "/undo"

opt.verbose = 0
opt.report = 99999
opt.shortmess:append "sIAc"
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
opt.relativenumber = true
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
opt.whichwrap:append "<,>,[,],h,l"
opt.iskeyword:append "-" -- treats words with `-` as single words
opt.formatoptions:remove { "c", "r", "o" } -- This is a sequence of letters which describes how automatic formatting is to be done
opt.linebreak = true

-------------------------------------- autocmds ------------------------------------------
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
    vim.cmd "startinsert!"
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
    vim.opt.formatoptions:remove { "c", "r", "o" }
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
      vim.cmd.wincmd "L"
    end
  end,
  desc = "Open help pages in a vertical split",
})

autocmd({ "VimEnter" }, {
  callback = function()
    vim.cmd "hi link illuminatedWord LspReferenceText"
  end,
})

autocmd({ "BufWinEnter" }, {
  callback = function()
    local line_count = vim.api.nvim_buf_line_count(0)
    if line_count >= 5000 then
      vim.cmd "IlluminatePauseBuf"
    end
  end,
})

-- highlight yanked text
autocmd("TextYankPost", {
  callback = function()
    require("vim.highlight").on_yank { higroup = "CurSearch", timeout = 300 }
  end,
  group = general,
  desc = "Highlight when yanking",
})

-- resize splits if window got resized
autocmd("VimResized", {
  callback = function()
    vim.cmd "wincmd ="
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
    if fn.line "'\"" > 1 and fn.line "'\"" <= fn.line "$" then
      vim.cmd 'normal! g`"'
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

autocmd("BufEnter", {
  desc = "Open Neo-Tree on startup with directory",
  group = augroup("neotree_start", { clear = true }),
  callback = function()
    local stats = vim.loop.fs_stat(vim.api.nvim_buf_get_name(0))
    if stats and stats.type == "directory" then
      require("neo-tree.setup.netrw").hijack()
    end
  end,
})

-- reload some chadrc options on-save
autocmd("BufWritePost", {
  pattern = vim.tbl_map(function(path)
    return vim.fs.normalize(vim.loop.fs_realpath(path))
  end, vim.fn.glob(vim.fn.stdpath "config" .. "/lua/custom/*.lua", true, true, true)),
  group = vim.api.nvim_create_augroup("ReloadNvChad", {}),

  callback = function(opts)
    local fp = vim.fn.fnamemodify(vim.fs.normalize(vim.api.nvim_buf_get_name(opts.buf)), ":r") --[[@as string]]
    local app_name = vim.env.NVIM_APPNAME and vim.env.NVIM_APPNAME or "nvim"
    local module = string.gsub(fp, "^.*/" .. app_name .. "/lua/", ""):gsub("/", ".")

    require("plenary.reload").reload_module "base46"
    require("plenary.reload").reload_module(module)
    require("plenary.reload").reload_module "custom.chadrc"

    config = require("core.utils").load_config()

    vim.g.nvchad_theme = config.ui.theme
    vim.g.transparency = config.ui.transparency

    -- statusline
    require("plenary.reload").reload_module("nvchad.statusline." .. config.ui.statusline.theme)
    vim.opt.statusline = "%!v:lua.require('nvchad.statusline." .. config.ui.statusline.theme .. "').run()"

    -- tabufline
    if config.ui.tabufline.enabled then
      require("plenary.reload").reload_module "nvchad.tabufline.modules"
      vim.opt.tabline = "%!v:lua.require('nvchad.tabufline.modules').run()"
    end

    require("base46").load_all_highlights()
    -- vim.cmd("redraw!")
  end,
})
-------------------------------------- commands ------------------------------------------
