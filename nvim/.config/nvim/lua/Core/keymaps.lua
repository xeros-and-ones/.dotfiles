-- Shorten function namE
local utils = require "utils"
local map = utils.map

-- Silent map option
local cwd = vim.fn.stdpath "config" .. "/"
local config_dir = { cwd }
--Remap space as leader key
map("", "<Space>", "<Nop>")
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

map("n", "<Up>", "<Nop>")
map("n", "<Down>", "<Nop>")
map("n", "<Left>", "<Nop>")
map("n", "<Right>", "<Nop>")

map("n", "<leader>w", "<cmd>w<cr>", { desc = "Save" })
map("n", "<leader>q", "<cmd>q<cr>", { desc = "Quit" })
map("n", "<S-q>", "<cmd>Bdelete<CR>")
map("n", "<A-q>", function()
    if vim.bo.buftype == "terminal" then
        vim.cmd "Bdelete!"
        vim.cmd "silent! close"
    elseif #vim.api.nvim_list_wins() > 1 then
        vim.cmd "silent! close"
    else
        vim.notify("Can't Close Window", vim.log.levels.WARN, { title = "Close Window" })
    end
end, { desc = "Close window" })

map({ "n", "i", "x" }, "<C-S>", "<Cmd>w<CR>", { desc = "Save" })
map("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear highlights" }) -- Clear highlights on ESC

-- Press jk fast to enter
map("i", "jk", "<ESC>")

-- Better window navigation
map("n", "<C-h>", "<C-w>h")
map("n", "<C-j>", "<C-w>j")
map("n", "<C-k>", "<C-w>k")
map("n", "<C-l>", "<C-w>l")

map("i", "<C-h>", "<Left>")
map("i", "<C-l>", "<Right>")
map("i", "<C-j>", "<Down>")
map("i", "<C-k>", "<Up>")

-- Resize with arrows
map("n", "<C-Up>", ":resize +2<CR>")
map("n", "<C-Down>", ":resize -2<CR>")
map("n", "<C-Left>", ":vertical resize +2<CR>")
map("n", "<C-Right>", ":vertical resize -2<CR>")

map({ "v", "n" }, "p", '"_dP')
map("x", "p", 'p:let @+=@0<CR>:let @"=@0<CR>')

-- ╔═════════════════════════════════════════════════╗
-- ║ Bufferline                                      ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<leader>bb", "<cmd>BufferLinePick<cr>", { desc = "Pick" })
map("n", "<leader>bc", "<cmd>BufferLinePickClose<cr>", { desc = "Pick close" })
map("n", "<leader>bq", "<cmd>Bdelete<cr>", { desc = "Close current" })
map("n", "<leader>bQ", function()
    local cur_buf = vim.fn.bufnr()
    for _, e in ipairs(require("bufferline").get_elements().elements) do
        vim.schedule(function()
            if e.id ~= cur_buf then
                vim.cmd("Bdelete " .. e.id)
            end
        end)
    end
end, { desc = "Close others" })
map("n", "<leader>bp", "<cmd>BufferLineTogglePin<cr>", { desc = "Pin" })
map("n", "<leader>bf", "<cmd>lua require('telescope.builtin').buffers()<cr>", { desc = "Find" })
map("n", "H", "<cmd>BufferLineCyclePrev<cr>", { desc = "Prev" })
map("n", "L", "<cmd>BufferLineCycleNext<cr>", { desc = "Next" })
map("n", "<leader>bh", "<cmd>BufferLineMovePrev<cr>", { desc = "Move Prev" })
map("n", "<leader>bl", "<cmd>BufferLineMoveNext<cr>", { desc = "Move Next" })

map("n", "<S-Left>", ":tabprevious<CR>", { desc = "Go to previous tab" })
map("n", "<S-Right>", ":tabnext<CR>", { desc = "Go to next tab" })
map("n", "<S-Up>", ":tabnew<CR>", { desc = "New tab" })
map("n", "<S-Down>", ":tabclose<CR>", { desc = "Close tab" })

for i = 1, 9 do
    map(
        "n",
        string.format("\\%d", i),
        string.format("<cmd>lua require'bufferline'.go_to(%d)<CR>", i),
        { desc = string.format("Buffer %d", i) }
    )
end

-- ╔═════════════════════════════════════════════════╗
-- ║ File Tree & Dashboard                                      ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<leader>e", "<cmd>Neotree toggle<cr>", { desc = "File Explorer" })
map("n", "<leader>za", function()
    require("alpha").start()
end, { desc = "Alpha Dashboard" })
-- ╔═════════════════════════════════════════════════╗
-- ║ Telescope                                       ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<leader>fw", function()
    require("telescope.builtin").live_grep {
        additional_args = function(args)
            return vim.list_extend(args, { "--hidden", "--no-ignore" })
        end,
    }
end, { desc = "Live Grep" })

map("n", "<leader>ff", function()
    require("telescope.builtin").find_files { hidden = true, no_ignore = true }
end, { desc = "Search all files" })
map("n", "<leader>fu", "<CMD>Telescope undo<CR>", { desc = "Find undo" })
map("n", "<leader>fb", function()
    require("telescope.builtin").buffers()
end, { desc = "Search buffers" })
map("n", "<leader>fm", function()
    require("telescope.builtin").marks()
end, { desc = "Search marks" })
map("n", "<leader>fr", function()
    require("telescope.builtin").oldfiles()
end, { desc = "Recent files" })
map("n", "<leader>fc", function()
    require("telescope.builtin").grep_string()
end, { desc = "Search for word under cursor" })
map("n", "<leader>fh", function()
    require("telescope.builtin").help_tags()
end, { desc = "Search help" })
map("n", "<leader>fH", ":Telescope highlights<cr>", { desc = "Highlights" })
map("n", "<leader>fM", function()
    require("telescope.builtin").man_pages()
end, { desc = "Search man" })
map("n", "<leader>fn", function()
    require("telescope").extensions.notify.notify()
end, { desc = "Search notifications" })
map("n", "<leader>fo", function()
    require("telescope.builtin").registers()
end, { desc = "Search registers" })
map("n", "<leader>fk", function()
    require("telescope.builtin").maps()
end, { desc = "Search maps" })
map("n", "<leader>fC", function()
    require("telescope.builtin").commands()
end, { desc = "Search commands" })
map("n", "<leader>ft", "<cmd>TodoTrouble<cr>", { desc = "TODO" })

map("n", "<leader>fp", ":Telescope projects<CR>", { desc = "Projects" })
map("n", "<leader>f/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", { desc = "Find in Buffer" })

-- ╔═════════════════════════════════════════════════╗
-- ║ Comment                                         ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<leader>/", "<cmd>lua require('Comment.api').toggle.linewise.current()<CR>", { desc = "Toggle Comment" })
map(
    "x",
    "<leader>/",
    "<esc><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>",
    { desc = "Toggle Comment" }
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
        { "<f6>", cmd "lua require('dap').step_over()", { exit = false, desc = "Step over" } },
        { "<f7>", cmd "lua require('dap').step_into()", { exit = false, desc = "Step into" } },
        { "<f8>", cmd "lua require('dap').step_out()", { exit = false, desc = "Step out" } },
        { "b", cmd "lua require('dap').toggle_breakpoint()", { exit = false, desc = "Toggle breakpoint" } },
        { "<cr>", cmd "lua require('dap').continue()", { exit = false, desc = "Continue" } },
        { "r", cmd "lua require('dap').repl.toggle()", { exit = false, desc = "REPL" } },
        { "q", cmd "lua require('dap').terminate()", { exit = true, desc = "Stop debugging" } },
        { "l", cmd "lua require('dap').run_last()", { exit = true, desc = "Run last" } },
        { "h", cmd "lua require('dap.ui.widgets').hover()", { exit = true, desc = "Hover" } },
        { "p", cmd "lua require('dap.ui.widgets').preview()", { exit = true, desc = "Preview" } },
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

-- ╔═════════════════════════════════════════════════╗
-- ║ neotest                                         ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<leader>ta", function()
    require("neotest").run.run { suite = true, strategy = "integrated" }
    require("neotest").summary.open()
end, { desc = "Run all tests" })
map("n", "<leader>tt", function()
    require("neotest").run.run { strategy = "integrated" }
end, { desc = "Run test" })
map("n", "<leader>td", function()
    require("neotest").run.run { strategy = "dap" }
end, { desc = "Debug test" })
map("n", "<leader>tr", function()
    require("neotest").run.run()
end, { desc = "Run Nearest" })
map("n", "<leader>ts", function()
    require("neotest").summary.toggle()
end, { desc = "Show summary" })
map("n", "<leader>to", function()
    require("neotest").output.open { enter = true, auto_close = true }
end, { desc = "Show output" })
map("n", "<leader>tO", function()
    require("neotest").output_panel.toggle()
end, { desc = "Show output" })
map("n", "<leader>tq", function()
    require("neotest").run.stop()
end, { desc = "Stop Tests" })

-- ╔═════════════════════════════════════════════════╗
-- ║ Terminal                                        ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<c-\\>", "<cmd>ToggleTerm direction=float<cr>")
map("t", "<c-\\>", "<cmd>ToggleTerm direction=float<cr>")
map("n", "<c-[>", "<cmd>ToggleTerm size=20 direction=horizontal<cr>")
map("t", "<c-[>", "<cmd>ToggleTerm size=20 direction=horizontal<cr>")
map("n", "<c-]>", "<cmd>ToggleTerm size=70 direction=vertical<cr>")
map("t", "<c-]>", "<cmd>ToggleTerm size=70 direction=vertical<cr>")

-- ╔═════════════════════════════════════════════════╗
-- ║ Git                                             ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<leader>gg", utils.ToggleLazygit, { desc = "Lazygit" })
map("n", "<leader>gt", function()
    require("telescope.builtin").git_status()
end, { desc = "Git status" })
map("n", "<leader>gb", function()
    require("telescope.builtin").git_branches()
end, { desc = "Git branches" })
map("n", "<leader>gc", function()
    require("telescope.builtin").git_commits()
end, { desc = "Git commits" })
map("n", "<leader>gc", function()
    if next(require("diffview.lib").views) == nil then
        vim.cmd "DiffviewOpen"
    else
        vim.cmd "DiffviewClose"
    end
end, { desc = "Toggle Diffview" })

-- ╔═════════════════════════════════════════════════╗
-- ║ ufo                                             ║
-- ╚═════════════════════════════════════════════════╝
map("n", "zR", function()
    require("ufo").openAllFolds()
end, { desc = "Open all folds" })
map("n", "zM", function()
    require("ufo").closeAllFolds()
end, { desc = "Close all folds" })
map("n", "zr", function()
    require("ufo").openFoldsExceptKinds()
end, { desc = "Open Folds" })
map("n", "zm", function()
    require("ufo").closeFoldsWith()
end, { desc = "close Folds" })
map("n", "zh", function()
    local winid = require("ufo").peekFoldedLinesUnderCursor()
    if not winid then
        return
    end
end, { desc = "Fold preview" })
-- ╔═════════════════════════════════════════════════╗
-- ║ Tools                                           ║
-- ╚═════════════════════════════════════════════════╝
map("n", "<leader>zl", "<cmd>Lazy<cr>", { desc = "Lazy" })
map("n", "<leader>zm", "<cmd>Mason<cr>", { desc = "Mason" })
map("n", "<leader>zi", "<cmd>LspInfo<cr>", { desc = "LspInfo" })
map("n", "<leader>zn", "<cmd>NullLsInfo<cr>", { desc = "Null-LS" })
map("n", "<leader>zc", "<cmd>CccPick<cr>", { desc = "Colour picker" })
map("n", "<leader>zs", "<cmd>lua require('luasnip.loaders').edit_snippet_files()<cr>", { desc = "Edit snippets" })
map("n", "<leader>zr", "<cmd>luafile %<CR>", { desc = "Source current file" })
map("n", "<leader>zt", "<cmd>Inspect<CR>", { desc = "TS Inspect" })
map("n", "<leader>zT", "<cmd>InspectTree<CR>", { desc = "TS Inspect Tree" })
map("n", "<leader>zO", "<cmd>OverseerToggle<cr>", { desc = "Overseer List" })
map("n", "<leader>zo", "<cmd>OverseerRun<cr>", { desc = "Overseer Run" })
map("n", "<Leader>zd", "<cmd>lua require('neogen').generate()<CR>", { desc = "Generate docs" })
map("n", "<Leader>zf", function()
    require("telescope.builtin").find_files {
        prompt_title = "Config Files",
        search_dirs = config_dir,
        cwd = cwd,
    }
end, { desc = "Find Config Files" })
map("n", "<Leader>zg", function()
    require("telescope.builtin").live_grep {
        prompt_title = "Config Files",
        search_dirs = config_dir,
        cwd = cwd,
    }
end, { desc = "Grep Config Files" })
map("n", "<Leader>zh", ":checkhealth<cr>", { desc = "Health" })
map("n", "<Leader>zv", function()
    local version = vim.version().major .. "." .. vim.version().minor .. "." .. vim.version().patch
    return vim.notify(version, vim.log.levels.INFO, { title = "Neovim Version" })
end, { desc = "Version" })

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
        vim.cmd "Lspsaga hover_doc"
    end
end

map("n", "<C-`>", "<cmd>TroubleToggle<cr>", { desc = "Toggle Trouble" })
map({ "n", "i", "v" }, "<C-Space>", "<cmd>lua vim.lsp.buf.format { async = true }<cr>")
map({ "n", "i", "v" }, "<F5>", utils.RunCode)
map("n", "gh", show_hover, { desc = "Hover Action" })
function LspMapping(bufnr)
    map("n", "gk", ":Lspsaga show_line_diagnostics<CR>", { desc = "Line Diagnostic", buffer = bufnr })
    map("n", "gp", ":Lspsaga peek_definition<CR>", { desc = "Peek_Definition", buffer = bufnr })
    map("n", "gi", ":Telescope lsp_implementations<CR>", { desc = "LSP implementation", buffer = bufnr })
    map({ "n", "v" }, "<leader>la", "<cmd>Lspsaga code_action<cr>", { desc = "Code Action", buffer = bufnr })
    map("n", "<leader>ld", "<cmd>Lspsaga goto_definition<cr>", { desc = "Goto_Definition", buffer = bufnr })
    map("n", "<leader>lO", "<cmd>Lspsaga outline<cr>", { desc = "Code Outline", buffer = bufnr })
    map("n", "<leader>li", "<cmd>Lspsaga incoming_calls<cr>", { desc = "Incoming Calls", buffer = bufnr })
    map("n", "<leader>lo", "<cmd>Lspsaga outgoing_calls<cr>", { desc = "Outgoing Calls" })
    map("n", "<leader>lj", "<cmd>Lspsaga diagnostic_jump_next<cr>", { desc = "Next Diagnostic", buffer = bufnr })
    map("n", "<leader>lk", "<cmd>Lspsaga diagnostic_jump_prev<cr>", { desc = "Prev Diagnostic", buffer = bufnr })
    map("n", "<leader>lR", "<cmd>LspRestart<cr>", { desc = "Restart LSP", buffer = bufnr })
    map("n", "<leader>lr", "<cmd>Lspsaga rename<cr>", { desc = "Rename", buffer = bufnr })
    map("n", "<leader>lF", "<cmd>Lspsaga finder tyd+ref+imp+def<cr>", { desc = "LspSaga Finder", buffer = bufnr })
    map("n", "<leader>lq", "<cmd>TroubleToggle quickfix<cr>", { desc = "Quickfix [Trouble]", buffer = bufnr })
    map("n", "<leader>lD", "<cmd>TroubleToggle lsp_definitions<cr>", { desc = "Definition [Trouble]", buffer = bufnr })
    map(
        "n",
        "<leader>lf",
        "<cmd>TroubleToggle lsp_references<cr>",
        { desc = "Find references [Trouble]", buffer = bufnr }
    )
    map(
        "n",
        "<leader>lt",
        "<cmd>TroubleToggle lsp_type_definitions<cr>",
        { desc = "Type Definition [Trouble]", buffer = bufnr }
    )
    map(
        "n",
        "<leader>lx",
        "<cmd>TroubleToggle document_diagnostics<cr>",
        { desc = "Buffer Diagnostics", buffer = bufnr }
    )
    map(
        "n",
        "<leader>lw",
        "<cmd>TroubleToggle workspace_diagnostics<cr>",
        { desc = "Workspace Diagnostics", buffer = bufnr }
    )
    map("n", "go", "<cmd>Telescope lsp_document_symbols<cr>", { desc = "Buffer Symbols" })
    map("n", "gO", "<cmd>Telescope lsp_workspace_symbols<cr>", { desc = "Workspace Symbols" })
    map("n", "<leader>lh", ":lua vim.lsp.inlay_hint(0, nil)<cr>", { desc = "Inlay Hint", buffer = bufnr })
    map("n", "<leader>ls", function()
        vim.lsp.buf.signature_help()
    end, { desc = "LSP signature help", buffer = bufnr })
    --  map('n',"gd",
    --     function()
    --         vim.lsp.buf.type_definition()
    --     end,
    --     {desc = "LSP definition type",
    -- })

    map("n", "gr", function()
        vim.lsp.buf.references()
    end, { desc = "LSP references", buffer = bufnr })

    map("n", "[d", function()
        vim.diagnostic.goto_prev { buffer = 0 }
    end, { desc = "Diagnostics Prev", buffer = bufnr })

    map("n", "]d", function()
        vim.diagnostic.goto_next { buffer = 0 }
    end, { desc = "Diagnostics Next", buffer = bufnr })

    map("n", "gD", function()
        vim.lsp.buf.declaration()
    end, { "LSP declaration", buffer = bufnr })
end
