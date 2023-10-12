-- Shorten function namE
local keymap = vim.keymap.set
-- Silent keymap option
local opts = { silent = true }

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

-- ╔═════════════════════════════════════════════════╗
-- ║ General                                         ║
-- ╚═════════════════════════════════════════════════╝
--moving text
-- Normal --
keymap("n", "<A-j>", "<ESC>:m .+1<CR>==", opts)
keymap("n", "<A-Down>", "<ESC>:m .+1<CR>==", opts)
keymap("n", "<A-k>", "<ESC>:m .-2<CR>==", opts)
keymap("n", "<A-Up>", "<ESC>:m .-2<CR>==", opts)

-- insert
-- Move text up and down
keymap("i", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("i", "<A-Down>", "<Esc>:m .+1<CR>==gi", opts)
keymap("i", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
keymap("i", "<A-Up>", "<Esc>:m .-2<CR>==gi", opts)

keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-Down>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "<A-Up>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)
-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-Down>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-Up>", ":move '<-2<CR>gv-gv", opts)

keymap("n", "<leader>w", "<cmd>w<cr>", { desc = "Save" })
keymap("n", "<leader>q", "<cmd>q<cr>", { desc = "Quit" })
keymap({ "n", "i", "x" }, "<C-S>", "<Cmd>w<CR>", { desc = "Save" })
keymap("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear highlights" }) -- Clear highlights on ESC

-- Better paste
keymap("v", "p", "P", opts)

-- Insert --
-- Press jk fast to enter
keymap("i", "jk", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize +2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize -2<CR>", opts)

-- ╔═════════════════════════════════════════════════╗
-- ║ Bufferline                                      ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>bb", "<cmd>BufferLinePick<cr>", { desc = "Pick" })
keymap("n", "<leader>bc", "<cmd>BufferLinePickClose<cr>", { desc = "Pick close" })
keymap("n", "<leader>bq", "<cmd>Bdelete!<cr>", { desc = "Close current" })
keymap("n", "<S-q>", "<cmd>Bdelete!<CR>", opts)
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
keymap("n", "<leader>bH", "<cmd>BufferLineMovePrev<cr>", { desc = "Move Prev" })
keymap("n", "<leader>bL", "<cmd>BufferLineMoveNext<cr>", { desc = "Move Next" })

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
    require("telescope.builtin").live_grep { search_dirs = { vim.fn.expand "%:p" } }
end, { desc = "Search words" })
keymap("n", "<leader>fW", function()
    require("telescope.builtin").live_grep {
        additional_args = function(args)
            return vim.list_extend(args, { "--hidden", "--no-ignore" })
        end,
    }
end, { desc = "Search words in all files" })
keymap("n", "<leader>gt", function()
    require("telescope.builtin").git_status()
end, { desc = "Git status" })
keymap("n", "<leader>gb", function()
    require("telescope.builtin").git_branches()
end, { desc = "Git branches" })
keymap("n", "<leader>gc", function()
    require("telescope.builtin").git_commits()
end, { desc = "Git commits" })
keymap("n", "<leader>ff", function()
    require("telescope.builtin").find_files { hidden = true, no_ignore = true }
end, { desc = "Search all files" })
keymap("n", "<leader>fb", function()
    require("telescope.builtin").buffers()
end, { desc = "Search buffers" })
keymap("n", "<leader>fm", function()
    require("telescope.builtin").marks()
end, { desc = "Search marks" })
keymap("n", "<leader>fo", function()
    require("telescope.builtin").oldfiles()
end, { desc = "Search history" })
keymap("n", "<leader>fc", function()
    require("telescope.builtin").grep_string()
end, { desc = "Search for word under cursor" })
keymap("n", "<leader>fh", function()
    require("telescope.builtin").help_tags()
end, { desc = "Search help" })
keymap("n", "<leader>fM", function()
    require("telescope.builtin").man_pages()
end, { desc = "Search man" })
keymap("n", "<leader>fn", function()
    require("telescope").extensions.notify.notify()
end, { desc = "Search notifications" })
keymap("n", "<leader>fr", function()
    require("telescope.builtin").registers()
end, { desc = "Search registers" })
keymap("n", "<leader>fk", function()
    require("telescope.builtin").keymaps()
end, { desc = "Search keymaps" })
keymap("n", "<leader>fC", function()
    require("telescope.builtin").commands()
end, { desc = "Search commands" })

keymap("n", "<leader>fp", ":Telescope projects<CR>", opts)

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
	_<f5>_: Continue      _b_: Breakpoint      _h_: Hover         _f_: Frames
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
        { "<f5>", cmd "lua require('dap').continue()",           { exit = true, desc = "Continue" } },
        { "<f6>", cmd "lua require('dap').step_over()",          { exit = false, desc = "Step over" } },
        { "<f7>", cmd "lua require('dap').step_into()",          { exit = false, desc = "Step into" } },
        { "<f8>", cmd "lua require('dap').step_out()",           { exit = false, desc = "Step out" } },
        { "q",    cmd "lua require('dap').terminate()",          { exit = true, desc = "Stop debugging" } },
        { "b",    cmd "lua require('dap').toggle_breakpoint()",  { exit = false, desc = "Toggle breakpoint" } },
        { "r",    cmd "lua require('dap').repl.open()",          { exit = true, desc = "REPL" } },
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

-- Lsp lines
keymap("n", "<leader>lL", function()
    require("lsp_lines").toggle()
end, { desc = "Toggle Lsp_lines" })

-- ╔═════════════════════════════════════════════════╗
-- ║ Code                                            ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<Leader>cd", "<cmd>lua require('neogen').generate()<CR>", { desc = "Generate docs" })
keymap("n", "<leader>cT", "<cmd>TodoTelescope<cr>", { desc = "TODO" })
keymap("n", "<leader>cta", function()
    require("neotest").run.run { suite = true, strategy = "integrated" }
    require("neotest").summary.open()
end, { desc = "Run all tests" })
keymap("n", "<leader>ctt", function()
    require("neotest").run.run { strategy = "integrated" }
end, { desc = "Run test" })
keymap("n", "<leader>ctd", function()
    require("neotest").run.run { strategy = "dap" }
end, { desc = "Debug test" })
keymap("n", "<leader>cts", function()
    require("neotest").summary.toggle()
end, { desc = "Show summary" })
keymap("n", "<leader>cto", function()
    require("neotest").output.open()
end, { desc = "Show output" })

-- ╔═════════════════════════════════════════════════╗
-- ║ Terminal                                        ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<leader>t", function()
    require("uts").UI_select {
        ["(⤢) Float"] = "vim.cmd('ToggleTerm direction=float')",
        ["(→) Vertical"] = "vim.cmd('ToggleTerm direction=vertical')",
        ["(↓) Horizontal"] = "vim.cmd('ToggleTerm size=20 direction=horizontal')",
    }
end, { desc = "Terminal" })

keymap("n", "<c-`>", "<cmd>ToggleTerm size=20 direction=horizontal<cr>")
keymap("t", "<c-`>", "<cmd>ToggleTerm size=20 direction=horizontal<cr>")
keymap("n", "<F7>", "<cmd>ToggleTerm size=70 direction=vertical<cr>")
keymap("t", "<F7>", "<cmd>ToggleTerm size=70 direction=vertical<cr>")
keymap("n", "<leader>gg", "<cmd>lua require('uts').toggle_lazygit()<cr>", { desc = "Lazygit" })

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
-- ╔═════════════════════════════════════════════════╗
-- ║ Search                                          ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "n", [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]])
keymap("n", "N", [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]])
keymap("n", "*", [[*<Cmd>lua require('hlslens').start()<CR>]])
keymap("n", "#", [[#<Cmd>lua require('hlslens').start()<CR>]])
keymap("n", "g*", [[g*<Cmd>lua require('hlslens').start()<CR>]])
keymap("n", "g#", [[g#<Cmd>lua require('hlslens').start()<CR>]])

-- ╔═════════════════════════════════════════════════╗
-- ║ Overseer                                        ║
-- ╚═════════════════════════════════════════════════╝
keymap("n", "<f5>", "<cmd>OverseerToggle<cr>", { desc = "Overseer List" })
keymap("n", "<c-f5>", "<cmd>OverseerRun<cr>", { desc = "Overseer Run" })
