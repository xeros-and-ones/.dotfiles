local actions = require("diffview.actions")

vim.api.nvim_create_user_command("DiffviewToggle", function(e)
	local view = require("diffview.lib").get_current_view()
	if view then
		vim.cmd("DiffviewClose")
	else
		vim.cmd("DiffviewOpen " .. e.args)
	end
end, { nargs = "*" })

require("diffview").setup({
	diff_binaries = false,
	enhanced_diff_hl = false,
	git_cmd = { "git" },
	use_icons = true,
	icons = {
		folder_closed = "",
		folder_open = "",
	},
	signs = {
		fold_closed = "",
		fold_open = "",
		done = "✓",
	},
	view = {
		default = {
			layout = "diff2_horizontal",
		},
		merge_tool = {
			layout = "diff3_horizontal",
			disable_diagnostics = true,
		},
		file_history = {

			layout = "diff2_horizontal",
		},
	},
	file_panel = {
		listing_style = "tree",
		tree_options = {
			flatten_dirs = true,
			folder_statuses = "only_folded",
		},
		win_config = {
			position = "left",
			width = 35,
		},
	},
	file_history_panel = {
		log_options = {
			git = {
				single_file = {
					max_count = 512,
					follow = true,
				},
				multi_file = {
					max_count = 128,
				},
			},
		},
		win_config = {
			position = "bottom",
			height = 16,
		},
	},
	commit_log_panel = {
		win_config = {},
	},
	default_args = {
		DiffviewOpen = {},
		DiffviewFileHistory = {},
	},
	hooks = {},
	keymaps = {
		disable_defaults = false, -- Disable the default keymaps
		view = {
			-- The `view` bindings are active in the diff buffers, only when the current
			-- tabpage is a Diffview.
			{
				"n",
				"<tab>",
				actions.select_next_entry,
				{ desc = "diff next file" },
			},
			{
				"n",
				"<s-tab>",
				actions.select_prev_entry,
				{ desc = "diff previous file" },
			},
			{
				"n",
				"gf",
				actions.goto_file_edit,
				{ desc = "file in previous tabpage" },
			},
			{ "n", "<C-w><C-f>", actions.goto_file_split, { desc = "Open in new split" } },
			{ "n", "<C-w>gf", actions.goto_file_tab, { desc = "Open file in new tabpage" } },
			{ "n", "<leader>e", actions.focus_files, { desc = "focus file panel" } },
			{ "n", "<leader>b", actions.toggle_files, { desc = "Toggle file panel." } },
			{
				"n",
				"g<C-x>",
				actions.cycle_layout,
				{ desc = "Cycle available layouts." },
			},
			{
				"n",
				"[x",
				actions.prev_conflict,
				{ desc = "merge-tool: jump to previous conflict" },
			},
			{
				"n",
				"]x",
				actions.next_conflict,
				{ desc = "merge-tool: jump to next conflict" },
			},
			{
				"n",
				"<leader>co",
				actions.conflict_choose("ours"),
				{ desc = "Choose OURS of a conflict" },
			},
			{
				"n",
				"<leader>ct",
				actions.conflict_choose("theirs"),
				{ desc = "Choose THEIRS of a conflict" },
			},
			{
				"n",
				"<leader>cb",
				actions.conflict_choose("base"),
				{ desc = "Choose BASE of a conflict" },
			},
			{
				"n",
				"<leader>ca",
				actions.conflict_choose("all"),
				{ desc = "Choose all of a conflict" },
			},
			{ "n", "dx", actions.conflict_choose("none"), { desc = "Delete conflict region" } },
			{
				"n",
				"<leader>cO",
				actions.conflict_choose_all("ours"),
				{ desc = "Choose OURS of a conflict (ALL)" },
			},
			{
				"n",
				"<leader>cT",
				actions.conflict_choose_all("theirs"),
				{ desc = "Choose THEIRS of a conflict (ALL)" },
			},
			{
				"n",
				"<leader>cB",
				actions.conflict_choose_all("base"),
				{ desc = "Choose BASE of a conflict (ALL)" },
			},
			{
				"n",
				"<leader>cA",
				actions.conflict_choose_all("all"),
				{ desc = "Choose all of a conflict (ALL)" },
			},
			{
				"n",
				"dX",
				actions.conflict_choose_all("none"),
				{ desc = "Delete conflict region (ALL)" },
			},
		},
		diff1 = {
			-- Mappings in single window diff layouts
			{ "n", "g?", actions.help({ "view", "diff1" }), { desc = "Open the help panel" } },
		},
		diff2 = {
			-- Mappings in 2-way diff layouts
			{ "n", "g?", actions.help({ "view", "diff2" }), { desc = "Open the help panel" } },
		},
		diff3 = {
			-- Mappings in 3-way diff layouts
			{
				{ "n", "x" },
				"2do",
				actions.diffget("ours"),
				{ desc = "diff hunk from OURS" },
			},
			{
				{ "n", "x" },
				"3do",
				actions.diffget("theirs"),
				{ desc = "diff hunk from THEIRS " },
			},
			{ "n", "g?", actions.help({ "view", "diff3" }), { desc = "Open the help panel" } },
		},
		diff4 = {
			-- Mappings in 4-way diff layouts
			{
				{ "n", "x" },
				"1do",
				actions.diffget("base"),
				{ desc = "diff hunk from BASE" },
			},
			{
				{ "n", "x" },
				"2do",
				actions.diffget("ours"),
				{ desc = "diff hunk from OURS" },
			},
			{
				{ "n", "x" },
				"3do",
				actions.diffget("theirs"),
				{ desc = "diff hunk from THEIRS" },
			},
			{ "n", "g?", actions.help({ "view", "diff4" }), { desc = "Open the help panel" } },
		},
		file_panel = {
			{
				"n",
				"j",
				actions.next_entry,
				{ desc = "cursor to next file" },
			},
			{
				"n",
				"<down>",
				actions.next_entry,
				{ desc = "cursor to next file" },
			},
			{
				"n",
				"k",
				actions.prev_entry,
				{ desc = "cursor to previous file" },
			},
			{
				"n",
				"<up>",
				actions.prev_entry,
				{ desc = "cursor to previous file " },
			},
			{
				"n",
				"<cr>",
				actions.select_entry,
				{ desc = "Open diff for selected" },
			},
			{
				"n",
				"o",
				actions.select_entry,
				{ desc = "Open diff for selected" },
			},
			{
				"n",
				"l",
				actions.select_entry,
				{ desc = "Open diff for selected" },
			},
			{
				"n",
				"<2-LeftMouse>",
				actions.select_entry,
				{ desc = "Open diff for selected" },
			},
			{
				"n",
				"-",
				actions.toggle_stage_entry,
				{ desc = "Stage / unstage: selected" },
			},
			{
				"n",
				"s",
				actions.toggle_stage_entry,
				{ desc = "Stage / unstage: selected" },
			},
			{ "n", "S", actions.stage_all, { desc = "Stage all entries" } },
			{ "n", "U", actions.unstage_all, { desc = "Unstage all entries" } },
			{
				"n",
				"X",
				actions.restore_entry,
				{ desc = "Restore entry to left side state" },
			},
			{ "n", "L", actions.open_commit_log, { desc = "Open commit log panel" } },
			{ "n", "zo", actions.open_fold, { desc = "Expand fold" } },
			{ "n", "h", actions.close_fold, { desc = "Collapse fold" } },
			{ "n", "zc", actions.close_fold, { desc = "Collapse fold" } },
			{ "n", "za", actions.toggle_fold, { desc = "Toggle fold" } },
			{ "n", "zR", actions.open_all_folds, { desc = "Expand all folds" } },
			{ "n", "zM", actions.close_all_folds, { desc = "Collapse all folds" } },
			{ "n", "<c-b>", actions.scroll_view(-0.25), { desc = "Scroll view up" } },
			{ "n", "<c-f>", actions.scroll_view(0.25), { desc = "Scroll view down" } },
			{
				"n",
				"<tab>",
				actions.select_next_entry,
				{ desc = "Open diff for next file" },
			},
			{
				"n",
				"<s-tab>",
				actions.select_prev_entry,
				{ desc = "Open diff for previous file" },
			},
			{
				"n",
				"gf",
				actions.goto_file_edit,
				{ desc = "Open file in previous tabpage" },
			},
			{
				"n",
				"<C-w><C-f>",
				actions.goto_file_split,
				{ desc = "Open file in new split" },
			},
			{
				"n",
				"<C-w>gf",
				actions.goto_file_tab,
				{ desc = "Open file in new tabpage" },
			},
			{
				"n",
				"i",
				actions.listing_style,
				{ desc = "Toggle 'list','tree' views" },
			},
			{
				"n",
				"f",
				actions.toggle_flatten_dirs,
				{ desc = "Flatten empty subdirectories" },
			},
			{
				"n",
				"R",
				actions.refresh_files,
				{ desc = "Update stats and entries" },
			},
			{
				"n",
				"<leader>e",
				actions.focus_files,
				{ desc = "focus file panel" },
			},
			{ "n", "<leader>b", actions.toggle_files, { desc = "Toggle the file panel" } },
			{ "n", "g<C-x>", actions.cycle_layout, { desc = "Cycle layouts" } },
			{ "n", "[x", actions.prev_conflict, { desc = "Go to previous conflict" } },
			{ "n", "]x", actions.next_conflict, { desc = "Go to next conflict" } },
			{ "n", "g?", actions.help("file_panel"), { desc = "Open the help panel" } },
			{
				"n",
				"<leader>cO",
				actions.conflict_choose_all("ours"),
				{ desc = "Choose OURS of a conflict (ALL)" },
			},
			{
				"n",
				"<leader>cT",
				actions.conflict_choose_all("theirs"),
				{ desc = "Choose THEIRS of a conflict (ALL)" },
			},
			{
				"n",
				"<leader>cB",
				actions.conflict_choose_all("base"),
				{ desc = "Choose BASE of a conflict (ALL)" },
			},
			{
				"n",
				"<leader>cA",
				actions.conflict_choose_all("all"),
				{ desc = "Choose all of a conflict (ALL)" },
			},
			{
				"n",
				"dX",
				actions.conflict_choose_all("none"),
				{ desc = "Delete conflict region (ALL)" },
			},
		},
		file_history_panel = {
			{ "n", "g!", actions.options, { desc = "Open the option panel" } },
			{
				"n",
				"<C-A-d>",
				actions.open_in_diffview,
				{ desc = "Open cursor entry in diffview" },
			},
			{
				"n",
				"y",
				actions.copy_hash,
				{ desc = "Copy commit hash under cursor" },
			},
			{ "n", "L", actions.open_commit_log, { desc = "Show commit details" } },
			{ "n", "zR", actions.open_all_folds, { desc = "Expand all folds" } },
			{ "n", "zM", actions.close_all_folds, { desc = "Collapse all folds" } },
			{
				"n",
				"j",
				actions.next_entry,
				{ desc = "Bring cursor to next file" },
			},
			{
				"n",
				"<down>",
				actions.next_entry,
				{ desc = "Bring cursor to next file" },
			},
			{
				"n",
				"k",
				actions.prev_entry,
				{ desc = "Bring cursor to previous file" },
			},
			{
				"n",
				"<up>",
				actions.prev_entry,
				{ desc = "Bring cursor to previous file" },
			},
			{
				"n",
				"<cr>",
				actions.select_entry,
				{ desc = "Open diff for selected" },
			},
			{
				"n",
				"o",
				actions.select_entry,
				{ desc = "Open diff for selected" },
			},
			{
				"n",
				"<2-LeftMouse>",
				actions.select_entry,
				{ desc = "Open diff for selected" },
			},
			{ "n", "<c-b>", actions.scroll_view(-0.25), { desc = "Scroll the view up" } },
			{ "n", "<c-f>", actions.scroll_view(0.25), { desc = "Scroll the view down" } },
			{ "n", "<tab>", actions.select_next_entry, { desc = "Open diff for next file" } },
			{
				"n",
				"<s-tab>",
				actions.select_prev_entry,
				{ desc = "Open diff for previous file" },
			},
			{
				"n",
				"gf",
				actions.goto_file_edit,
				{ desc = "Open file in previous tabpage" },
			},
			{ "n", "<C-w><C-f>", actions.goto_file_split, { desc = "Open file in new split" } },
			{ "n", "<C-w>gf", actions.goto_file_tab, { desc = "Open file in new tabpage" } },
			{ "n", "<leader>e", actions.focus_files, { desc = "Bring focus to file panel" } },
			{ "n", "<leader>b", actions.toggle_files, { desc = "Toggle file panel" } },
			{ "n", "g<C-x>", actions.cycle_layout, { desc = "Cycle layouts" } },
			{ "n", "g?", actions.help("file_history_panel"), { desc = "Open the help panel" } },
		},
		option_panel = {
			{ "n", "<tab>", actions.select_entry, { desc = "Change current option" } },
			{ "n", "q", actions.close, { desc = "Close the panel" } },
			{ "n", "g?", actions.help("option_panel"), { desc = "Open the help panel" } },
		},
		help_panel = {
			{ "n", "q", actions.close, { desc = "Close help menu" } },
			{ "n", "<esc>", actions.close, { desc = "Close help menu" } },
		},
	},
})
