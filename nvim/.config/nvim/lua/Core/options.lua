local opt = vim.opt

opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.textwidth = 88

opt.breakindent = true

opt.foldenable = true
opt.foldcolumn = "0"
opt.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
opt.foldlevelstart = 99

opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = true
opt.infercase = true
opt.smartcase = true

opt.pumblend = 0                            -- Make builtin completion menus slightly transparent
opt.pumheight = 10                          -- Make popup menu smaller
opt.completeopt = { "menuone", "noselect" } -- mostly just for cmp
-- opt.winblend = 0
opt.mousemoveevent = false                  -- screws with toggleterm input

opt.autochdir = false
opt.undofile = true
opt.undodir = vim.fn.stdpath "data" .. "/undo"

opt.verbose = 0
opt.report = 99999
opt.shortmess:append "astWAIcFS"
opt.sessionoptions = "buffers,curdir,folds,tabpages,winsize,winpos,localoptions,globals,options"

-- Yank to system clipboard
opt.clipboard = "unnamedplus"              -- allows neovim to access the system clipboard

opt.backup = false                         -- creates a backup file
opt.cmdheight = 1                          -- more space in the neovim command line for displaying messages
opt.conceallevel = 0                       -- so that `` is visible in markdown files
opt.fileencoding = "utf-8"                 -- the encoding written to a file
opt.hlsearch = true                        -- highlight all matches on previous search pattern
opt.mouse = "a"                            -- allow the mouse to be used in neovim
opt.showmode = false                       -- we don't need to see things like -- INSERT -- anymore
opt.showtabline = 0                        -- always show tabs
opt.smartindent = true                     -- make indenting smarter again
opt.splitbelow = true                      -- force all horizontal splits to go below current window
opt.splitright = true                      -- force all vertical splits to go to the right of current window
opt.swapfile = false                       -- creates a swapfile
opt.termguicolors = true                   -- set term gui colors (most terminals support this)
opt.timeout = true
opt.timeoutlen = 300                       -- time to wait for a mapped sequence to complete (in milliseconds)
opt.updatetime = 250                       -- faster completion (4000ms default)
opt.writebackup = false                    -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
opt.expandtab = true                       -- convert tabs to spaces
opt.cursorline = true                      -- highlight the current line
opt.number = true                          -- set numbered lines
opt.relativenumber = true
opt.laststatus = 3                         -- only the last window will always have a status line
opt.showcmd = false                        -- hide (partial) command in the last line of the screen (for performance)
opt.ruler = false                          -- hide the line and column number of the cursor position
opt.numberwidth = 4                        -- minimal number of columns to use for the line number {default 4}
opt.signcolumn = "yes"                     -- always show the sign column, otherwise it would shift the text each time
opt.wrap = false                           -- display lines as one long line
opt.scrolloff = 8                          -- minimal number of screen lines to keep above and below the cursor
opt.sidescrolloff = 8                      -- minimal number of screen columns to keep to the left and right of the cursor if wrap is `false`
opt.guifont = "monospace:h17"              -- the font used in graphical neovim applications
opt.fillchars = { eob = " " }              -- show empty lines at the end of a buffer as ` ` {default `~`}
opt.fillchars:append { diff = "╱" }
opt.whichwrap:append "[,],b,s"             -- keys allowed to move to the previous/next line when the beginning/end of line is reached
opt.iskeyword:append "-"                   -- treats words with `-` as single words
opt.formatoptions:remove { "c", "r", "o" } -- This is a sequence of letters which describes how automatic formatting is to be done
opt.linebreak = true

vim.diagnostic.config {
    -- disable virtual text
    virtual_text = false,
    update_in_insert = true,
    underline = true,
    severity_sort = true,
    float = {
        focusable = false,
        style = "minimal",
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
        suffix = "",
    },
}
