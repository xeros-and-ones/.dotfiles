-- Shorten function namE
local keymap = vim.keymap.set
-- Silent keymap option
local opts = { silent = true }
local cwd = vim.fn.stdpath "config" .. "/"
local config_dir = { cwd }
--Remap space as leader key
keymap("", "<Space>", "<Nop>")
vim.g.mapleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",
local ToggleLazygit = function()
    local Terminal = require("toggleterm.terminal").Terminal
    local lazygit = Terminal:new {
        cmd = "lazygit",
        hidden = true,
        on_open = function(term)
            vim.cmd "startinsert!"
        end,
        close_on_exit = true,
        direction = "float",
        float_opts = {
            border = "rounded",
        },
    }
    lazygit:toggle()
end

local function substitute(cmd)
    cmd = cmd:gsub("%%", vim.fn.expand "%")
    cmd = cmd:gsub("$fileBase", vim.fn.expand "%:r")
    cmd = cmd:gsub("$filePath", vim.fn.expand "%:p")
    cmd = cmd:gsub("$file", vim.fn.expand "%")
    cmd = cmd:gsub("$dir", vim.fn.expand "%:p:h")
    cmd = cmd:gsub("#", vim.fn.expand "#")
    cmd = cmd:gsub("$altFile", vim.fn.expand "#")

    return cmd
end
local RunCode = function()
    local file_extension = vim.fn.expand "%:e"
    local selected_cmd = ""
    local term_cmd = "bot 10 new | term "
    local supported_filetypes = {
        html = {
            default = "%",
        },
        c = {
            default = "gcc % -o $fileBase && $fileBase",
            debug = "gcc -g % -o $fileBase && $fileBase",
        },
        cs = {
            default = "dotnet run",
        },
        cpp = {
            default = "g++ % -o  $fileBase && $fileBase",
            debug = "g++ -g % -o  $fileBase",
            competitive = "g++ -std=c++17 -Wall -DAL -O2 % -o $fileBase && $fileBase<input.txt",
        },
        py = {
            default = "python %",
        },
        go = {
            default = "go run %",
        },
        java = {
            default = "java %",
        },
        js = {
            default = "node %",
            debug = "node --inspect %",
        },
        ts = {
            default = "tsc % && node $fileBase",
        },
        rs = {
            default = "rustc % && $fileBase",
        },
        php = {
            default = "php %",
        },
        r = {
            default = "Rscript %",
        },
        jl = {
            default = "julia %",
        },
        rb = {
            default = "ruby %",
        },
        pl = {
            default = "perl %",
        },
    }

    if supported_filetypes[file_extension] then
        local choices = vim.tbl_keys(supported_filetypes[file_extension])

        if #choices == 0 then
            vim.notify("It doesn't contain any command", vim.log.levels.WARN, { title = "Code Runner" })
        elseif #choices == 1 then
            selected_cmd = supported_filetypes[file_extension][choices[1]]
            vim.cmd(term_cmd .. substitute(selected_cmd))
        else
            vim.ui.select(choices, { prompt = "Choose a command: " }, function(choice)
                selected_cmd = supported_filetypes[file_extension][choice]
                if selected_cmd then
                    vim.cmd(term_cmd .. substitute(selected_cmd))
                end
            end)
        end
    else
        vim.notify("The filetype isn't included in the list", vim.log.levels.WARN, { title = "Code Runner" })
    end
end
-- ╔═════════════════════════════════════════════════╗
-- ║ General                                         ║
-- ╚═════════════════════════════════════════════════╝

keymap("n", "<Up>", "<Nop>")
keymap("n", "<Down>", "<Nop>")
keymap("n", "<Left>", "<Nop>")
keymap("n", "<Right>", "<Nop>")

keymap("n", "<leader>w", "<cmd>w<cr>", { desc = "Save" })
keymap("n", "<leader>q", "<cmd>q<cr>", { desc = "Quit" })
keymap("n", "<S-q>", "<cmd>Bdelete<CR>", opts)
keymap("n", "<A-q>", function()
    if vim.bo.buftype == "terminal" then
        vim.cmd "Bdelete!"
        vim.cmd "silent! close"
    elseif #vim.api.nvim_list_wins() > 1 then
        vim.cmd "silent! close"
    else
        vim.notify("Can't Close Window", vim.log.levels.WARN, { title = "Close Window" })
    end
end, { desc = "Close window", silent = true })

keymap({ "n", "i", "x" }, "<C-S>", "<Cmd>w<CR>", { desc = "Save" })
keymap("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear highlights" }) -- Clear highlights on ESC

-- Press jk fast to enter
keymap("i", "jk", "<ESC>", opts)

-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

keymap("i", "<C-h>", "<Left>", opts)
keymap("i", "<C-l>", "<Right>", opts)
keymap("i", "<C-j>", "<Down>", opts)
keymap("i", "<C-k>", "<Up>", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize +2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize -2<CR>", opts)

keymap("v", "p", '"_dP', opts)
keymap("x", "p", 'p:let @+=@0<CR>:let @"=@0<CR>', opts)

keymap({ "n", "i", "v" }, "<C-Space>", "<cmd>lua vim.lsp.buf.format { async = true }<cr>", opts)

keymap({ "n", "i", "v" }, "<F5>", RunCode, opts)

-- ╔═════════════════════════════════════════════════╗
-- ║ Bufferline                                      ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>bb", "<cmd>BufferLinePick<cr>", { desc = "Pick" })
keymap("n", "<leader>bc", "<cmd>BufferLinePickClose<cr>", { desc = "Pick close" })
keymap("n", "<leader>bq", "<cmd>Bdelete<cr>", { desc = "Close current" })
keymap("n", "<leader>bQ", function()
    local cur_buf = vim.fn.bufnr()
    for _, e in ipairs(require("bufferline").get_elements().elements) do
        vim.schedule(function()
            if e.id ~= cur_buf then
                vim.cmd("bd " .. e.id)
            end
        end)
    end
end, { desc = "Close others" })
keymap("n", "<leader>bp", "<cmd>BufferLineTogglePin<cr>", { desc = "Pin" })
keymap("n", "<leader>bf", "<cmd>lua require('telescope.builtin').buffers()<cr>", { desc = "Find" })
keymap("n", "H", "<cmd>BufferLineCyclePrev<cr>", { desc = "Prev" })
keymap("n", "L", "<cmd>BufferLineCycleNext<cr>", { desc = "Next" })
keymap("n", "<leader>bh", "<cmd>BufferLineMovePrev<cr>", { desc = "Move Prev" })
keymap("n", "<leader>bl", "<cmd>BufferLineMoveNext<cr>", { desc = "Move Next" })

keymap("n", "<S-Left>", ":tabprevious<CR>", { desc = "Go to previous tab", silent = true })
keymap("n", "<S-Right>", ":tabnext<CR>", { desc = "Go to next tab", silent = true })
keymap("n", "<S-Up>", ":tabnew<CR>", { desc = "New tab", silent = true })
keymap("n", "<S-Down>", ":tabclose<CR>", { desc = "Close tab", silent = true })

for i = 1, 9 do
    keymap(
        "n",
        string.format("\\%d", i),
        string.format("<cmd>lua require'bufferline'.go_to(%d)<CR>", i),
        { desc = string.format("Buffer %d", i) }
    )
end

-- ╔═════════════════════════════════════════════════╗
-- ║ File Tree & Dashboard                                      ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>e", "<cmd>Neotree toggle<cr>", { desc = "File Explorer" })
keymap("n", "<leader>za", function()
    require("alpha").start()
end, { desc = "Alpha Dashboard" })
-- ╔═════════════════════════════════════════════════╗
-- ║ Telescope                                       ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>fw", function()
    require("telescope.builtin").live_grep {
        additional_args = function(args)
            return vim.list_extend(args, { "--hidden", "--no-ignore" })
        end,
    }
end, { desc = "Live Grep" })

keymap("n", "<leader>ff", function()
    require("telescope.builtin").find_files { hidden = true, no_ignore = true }
end, { desc = "Search all files" })
keymap("n", "<leader>fu", "<CMD>Telescope undo<CR>", { desc = "Find undo" })
keymap("n", "<leader>fb", function()
    require("telescope.builtin").buffers()
end, { desc = "Search buffers" })
keymap("n", "<leader>fm", function()
    require("telescope.builtin").marks()
end, { desc = "Search marks" })
keymap("n", "<leader>fr", function()
    require("telescope.builtin").oldfiles()
end, { desc = "Recent files" })
keymap("n", "<leader>fc", function()
    require("telescope.builtin").grep_string()
end, { desc = "Search for word under cursor" })
keymap("n", "<leader>fh", function()
    require("telescope.builtin").help_tags()
end, { desc = "Search help" })
keymap("n", "<leader>fH", ":Telescope highlights<cr>", { desc = "Highlights" })
keymap("n", "<leader>fM", function()
    require("telescope.builtin").man_pages()
end, { desc = "Search man" })
keymap("n", "<leader>fn", function()
    require("telescope").extensions.notify.notify()
end, { desc = "Search notifications" })
keymap("n", "<leader>fo", function()
    require("telescope.builtin").registers()
end, { desc = "Search registers" })
keymap("n", "<leader>fk", function()
    require("telescope.builtin").keymaps()
end, { desc = "Search keymaps" })
keymap("n", "<leader>fC", function()
    require("telescope.builtin").commands()
end, { desc = "Search commands" })
keymap("n", "<leader>ft", "<cmd>TodoTrouble<cr>", { desc = "TODO" })

keymap("n", "<leader>fp", ":Telescope projects<CR>", { desc = "Projects" })
keymap("n", "<leader>f/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", { desc = "Find in Buffer" })

-- ╔═════════════════════════════════════════════════╗
-- ║ Comment                                         ║
-- ╚═════════════════════════════════════════════════╝
keymap(
    "n",
    "<leader>/",
    "<cmd>lua require('Comment.api').toggle.linewise.current()<CR>",
    { desc = "Toggle Comment", silent = true }
)
keymap(
    "x",
    "<leader>/",
    "<esc><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>",
    { desc = "Toggle Comment", silent = true }
)

-- ╔═════════════════════════════════════════════════╗
-- ║ Debug                                           ║
-- ╚═════════════════════════════════════════════════╝
local Hydra = require "hydra"
local cmd = require("hydra.keymap-util").cmd
local hint = [[
	_<cr>_: Continue      _b_: Breakpoint      _h_: Hover         _f_: Frames
	_<f6>_: Step over     _r_: REPL            _p_: Preview       _s_: Scopes
	_<f7>_: Step into     _l_: Run last
	_<f8>_: Step out                                          ^_q_: Quit
	]]
Hydra {
    name = "Debug",
    hint = hint,
    config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
            position = "bottom",
            border = "rounded",
        },
    },
    mode = { "n", "x" },
    body = "<leader>d",
    heads = {
        { "<f6>", cmd "lua require('dap').step_over()",          { exit = false, desc = "Step over" } },
        { "<f7>", cmd "lua require('dap').step_into()",          { exit = false, desc = "Step into" } },
        { "<f8>", cmd "lua require('dap').step_out()",           { exit = false, desc = "Step out" } },
        { "b",    cmd "lua require('dap').toggle_breakpoint()",  { exit = false, desc = "Toggle breakpoint" } },
        { "<cr>", cmd "lua require('dap').continue()",           { exit = false, desc = "Continue" } },
        { "r",    cmd "lua require('dap').repl.toggle()",        { exit = false, desc = "REPL" } },
        { "q",    cmd "lua require('dap').terminate()",          { exit = true, desc = "Stop debugging" } },
        { "l",    cmd "lua require('dap').run_last()",           { exit = true, desc = "Run last" } },
        { "h",    cmd "lua require('dap.ui.widgets').hover()",   { exit = true, desc = "Hover" } },
        { "p",    cmd "lua require('dap.ui.widgets').preview()", { exit = true, desc = "Preview" } },
        {
            "f",
            function()
                local widgets = require "dap.ui.widgets"
                widgets.centered_float(widgets.frames)
            end,
            { exit = true, desc = "Frames" },
        },
        {
            "s",
            function()
                local widgets = require "dap.ui.widgets"
                widgets.centered_float(widgets.scopes)
            end,
            { exit = true, desc = "Scopes" },
        },
        { "<Esc>", nil, { exit = true, nowait = true, desc = false } },
    },
}
-- ╔═════════════════════════════════════════════════╗
-- ║ LSP                                             ║
-- ╚═════════════════════════════════════════════════╝
local function show_hover()
    local filetype = vim.bo.filetype
    if vim.tbl_contains({ "vim", "help" }, filetype) then
        vim.cmd("h " .. vim.fn.expand "<cword>")
    elseif vim.tbl_contains({ "man" }, filetype) then
        vim.cmd("Man " .. vim.fn.expand "<cword>")
    elseif vim.fn.expand "%:t" == "Cargo.toml" and require("crates").popup_available() then
        require("crates").show_versions_popup()
    else
        vim.lsp.buf.hover()
    end
end
keymap("n", "gh", show_hover, { desc = "Hover", silent = true }) -- mapped outside otherwise types w/o LSP won't get the bind
keymap("n", "<c-`>", "<cmd>TroubleToggle<cr>", opts)
keymap("n", "<F5>", "<cmd>TermExec cmd='python %' go_back=0 direction=horizontal<cr>", opts)

-- ╔═════════════════════════════════════════════════╗
-- ║ neotest                                         ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>ta", function()
    require("neotest").run.run { suite = true, strategy = "integrated" }
    require("neotest").summary.open()
end, { desc = "Run all tests" })
keymap("n", "<leader>tt", function()
    require("neotest").run.run { strategy = "integrated" }
end, { desc = "Run test" })
keymap("n", "<leader>td", function()
    require("neotest").run.run { strategy = "dap" }
end, { desc = "Debug test" })
keymap("n", "<leader>tr", function()
    require("neotest").run.run()
end, { desc = "Run Nearest" })
keymap("n", "<leader>ts", function()
    require("neotest").summary.toggle()
end, { desc = "Show summary" })
keymap("n", "<leader>to", function()
    require("neotest").output.open { enter = true, auto_close = true }
end, { desc = "Show output" })
keymap("n", "<leader>tO", function()
    require("neotest").output_panel.toggle()
end, { desc = "Show output" })
keymap("n", "<leader>tq", function()
    require("neotest").run.stop()
end, { desc = "Stop Tests" })

-- ╔═════════════════════════════════════════════════╗
-- ║ Terminal                                        ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<c-\\>", "<cmd>ToggleTerm direction=float<cr>")
keymap("t", "<c-\\>", "<cmd>ToggleTerm direction=float<cr>")
keymap("n", "<c-[>", "<cmd>ToggleTerm size=20 direction=horizontal<cr>")
keymap("t", "<c-[>", "<cmd>ToggleTerm size=20 direction=horizontal<cr>")
keymap("n", "<c-]>", "<cmd>ToggleTerm size=70 direction=vertical<cr>")
keymap("t", "<c-]>", "<cmd>ToggleTerm size=70 direction=vertical<cr>")

-- ╔═════════════════════════════════════════════════╗
-- ║ Git                                             ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>gg", ToggleLazygit, { desc = "Lazygit" })
keymap("n", "<leader>gt", function()
    require("telescope.builtin").git_status()
end, { desc = "Git status" })
keymap("n", "<leader>gb", function()
    require("telescope.builtin").git_branches()
end, { desc = "Git branches" })
keymap("n", "<leader>gc", function()
    require("telescope.builtin").git_commits()
end, { desc = "Git commits" })
keymap("n", "<leader>gc", function()
    if next(require("diffview.lib").views) == nil then
        vim.cmd "DiffviewOpen"
    else
        vim.cmd "DiffviewClose"
    end
end, { desc = "Toggle Diffview" })

-- ╔═════════════════════════════════════════════════╗
-- ║ ufo                                             ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "zR", function()
    require("ufo").openAllFolds()
end, { desc = "Open all folds" })
keymap("n", "zM", function()
    require("ufo").closeAllFolds()
end, { desc = "Close all folds" })
keymap("n", "zr", function()
    require("ufo").openFoldsExceptKinds()
end, { desc = "Open Folds" })
keymap("n", "zm", function()
    require("ufo").closeFoldsWith()
end, { desc = "close Folds" })
keymap("n", "zh", function()
    local winid = require("ufo").peekFoldedLinesUnderCursor()
    if not winid then
        return
    end
end, { desc = "Fold preview" })
-- ╔═════════════════════════════════════════════════╗
-- ║ Tools                                           ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>zl", "<cmd>Lazy<cr>", { desc = "Lazy" })
keymap("n", "<leader>zm", "<cmd>Mason<cr>", { desc = "Mason" })
keymap("n", "<leader>zi", "<cmd>LspInfo<cr>", { desc = "LspInfo", noremap = true, silent = true })
keymap("n", "<leader>zn", "<cmd>NullLsInfo<cr>", { desc = "Null-LS", noremap = true, silent = true })
keymap("n", "<leader>zc", "<cmd>CccPick<cr>", { desc = "Colour picker" })
keymap("n", "<leader>zs", "<cmd>lua require('luasnip.loaders').edit_snippet_files()<cr>", { desc = "Edit snippets" })
keymap("n", "<leader>zr", "<cmd>luafile %<CR>", { desc = "Source current file" })
keymap("n", "<leader>zt", "<cmd>Inspect<CR>", { desc = "TS Inspect" })
keymap("n", "<leader>zT", "<cmd>InspectTree<CR>", { desc = "TS Inspect Tree" })
keymap("n", "<leader>zO", "<cmd>OverseerToggle<cr>", { desc = "Overseer List" })
keymap("n", "<leader>zo", "<cmd>OverseerRun<cr>", { desc = "Overseer Run" })
keymap("n", "<Leader>zd", "<cmd>lua require('neogen').generate()<CR>", { desc = "Generate docs" })
keymap("n", "<Leader>zf", function()
    require("telescope.builtin").find_files {
        prompt_title = "Config Files",
        search_dirs = config_dir,
        cwd = cwd,
    }
end, { desc = "Find Config Files", silent = true })
keymap("n", "<Leader>zg", function()
    require("telescope.builtin").live_grep {
        prompt_title = "Config Files",
        search_dirs = config_dir,
        cwd = cwd,
    }
end, { desc = "Grep Config Files", silent = true })
keymap("n", "<Leader>zh", ":checkhealth<cr>", { desc = "Health", silent = true })
keymap("n", "<Leader>zv", function()
    local version = vim.version().major .. "." .. vim.version().minor .. "." .. vim.version().patch
    return vim.notify(version, vim.log.levels.INFO, { title = "Neovim Version" })
end, { desc = "Version", silent = true })
