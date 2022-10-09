-- Shorten function name
local keymap = vim.keymap.set
-- Silent keymap option
local opts = { noremap = true, silent = true }

--Remap space as leader key
vim.g.mapleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

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
-- visual
-- move text up and down
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-Down>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "<A-Up>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)
-- Visual Block --
-- Move text up and down
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-Down>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-Up>", ":move '<-2<CR>gv-gv", opts)
--
--
--
--
--
-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Clear highlights
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>",{desc = "No Highlights"}, opts)

--save buffer
keymap("n", "<leader>w", "<cmd>w<cr>", {desc = "Save Buffer"}, opts)

-- Close buffers
keymap("n", "<leader>q", "<cmd>q<CR>", {desc = "Quit"}, opts)

-- Stay in indent mode
keymap("n", "<", "<cmd><<cr>", opts)
keymap("n", ">", "<cmd>><cr>", opts)

-- Better paste
keymap("v", "p", '"_dP', opts)

-- Insert --
-- Press jk fast to enter
keymap("i", "jk", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Plugins --

-- NeoTree
keymap("n", "<leader>e", ":NvimTreeToggle<CR>", {desc = "Toggle Explorer"}, opts)
keymap("n", "<leader>t", ":NvimTreeFocus<CR>", {desc = "Float Explorer"}, opts)

-- Telescope
keymap("n", "<leader>ff",
  "<cmd>lua require('telescope.builtin').find_files({hiddin = true, no_ignore = true})<CR>",
  {desc = "Find Files"},
  opts
)
keymap("n", "<leader>fw",
  "<cmd>lua require('telescope.builtin').live_grep({ search_dirs = { vim.fn.expand('%:p') } })<cr>",
  {desc = "Find text In Buffer"},
  opts
)
keymap("n", "<leader>fW",
  "<cmd>lua require('telescope.builtin').live_grep({ additional_args = function(args) return vim.list_extend(args, { '--hidden', '--no-ignore' })end,})<cr>",
  {desc = "Find Text In workspace"},
  opts
)
keymap("n", "<leader>fh",
  "<cmd>lua require('telescope.builtin').help_tags()<CR>",
  {desc = "Search Help"},
  opts
)
keymap("n", "<leader>fp", ":Telescope projects<CR>", {desc = "Search Projects"}, opts)
keymap("n", "<leader>fb", "<cmd>lua require('telescope.builtin').buffers()<CR>", {desc = "Search Buffers"}, opts)
keymap("n", "<leader>gt",
  "<cmd>lua require('telescope.builtin').git_status()<CR>",
  {desc = "Git Status"},
  opts
)
keymap("n", "<leader>gb",
  "<cmd>lua require('telescope.builtin').git_branches()<CR>",
  {desc = "Git Repo Branches"},
  opts
)
keymap("n", "<leader>gc",
  "<cmd>lua require('telescope.builtin').git_commits()<CR>",
  {desc = "Git Commits"},
  opts
)

-- Git
keymap("n", "<leader>gg", "<cmd>lua _LAZYGIT_TOGGLE()<CR>", opts)

-- Comment
keymap("n", "<leader>/", "<cmd>lua require('Comment.api').toggle_current_linewise()<CR>", opts)
keymap("x", "<leader>/", '<ESC><CMD>lua require("Comment.api").toggle_linewise_op(vim.fn.visualmode())<CR>')

-- DAP
keymap("n", "<leader>db", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", opts)
keymap("n", "<leader>dc", "<cmd>lua require'dap'.continue()<cr>", opts)
keymap("n", "<leader>di", "<cmd>lua require'dap'.step_into()<cr>", opts)
keymap("n", "<leader>do", "<cmd>lua require'dap'.step_over()<cr>", opts)
keymap("n", "<leader>dO", "<cmd>lua require'dap'.step_out()<cr>", opts)
keymap("n", "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>", opts)
keymap("n", "<leader>dl", "<cmd>lua require'dap'.run_last()<cr>", opts)
keymap("n", "<leader>du", "<cmd>lua require'dapui'.toggle()<cr>", opts)
keymap("n", "<leader>dt", "<cmd>lua require'dap'.terminate()<cr>", opts)
