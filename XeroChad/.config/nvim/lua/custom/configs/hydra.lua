local Hydra = require("hydra")
local gitsigns = require("gitsigns")
local cmd = require("hydra.keymap-util").cmd
local git_hint = [[
_]_: next hunk    _s_: stage hunk       _d_: show deleted     _b_: blame line
_[_: prev hunk    _S_: stage buffer     _p_: preview hunk     _B_: toggle blame line
_D_: diff view    _R_: reset buffer     _r_: reset hunk       _u_: undo last stage
_g_: Open LazyGit ^ ^
]]
Hydra({
	name = "Git",
	hint = git_hint,
	config = {
		color = "pink",
		invoke_on_body = true,
		hint = {
			position = "bottom",
			border = "rounded",
		},
		on_enter = function()
			vim.cmd("mkview")
			vim.cmd("silent! %foldopen!")
			vim.bo.modifiable = false
			gitsigns.toggle_linehl(true)
			gitsigns.toggle_signs(true)
			gitsigns.toggle_deleted(false)
		end,
		on_exit = function()
			local cursor_pos = vim.api.nvim_win_get_cursor(0)
			vim.cmd("loadview")
			vim.api.nvim_win_set_cursor(0, cursor_pos)
			vim.cmd("normal zv")
			gitsigns.toggle_linehl(false)
			gitsigns.toggle_deleted(false)
		end,
	},
	mode = { "n", "x" },
	body = "<leader>gh",
	heads = {
		{
			"]",
			function()
				if vim.wo.diff then
					return "]c"
				end
				vim.schedule(function()
					gitsigns.next_hunk()
				end)
				return "<Ignore>"
			end,
			{ expr = true, desc = "next hunk" },
		},
		{
			"[",
			function()
				if vim.wo.diff then
					return "[c"
				end
				vim.schedule(function()
					gitsigns.prev_hunk()
				end)
				return "<Ignore>"
			end,
			{ expr = true, desc = "prev hunk" },
		},
		{ "D", gitsigns.diffthis, { exit = false, silent = true, desc = "diff view" } },
		{ "s", cmd("Gitsigns stage_hunk"), { silent = true, desc = "stage hunk" } },
		{ "u", gitsigns.undo_stage_hunk, { desc = "undo last stage" } },
		{ "S", gitsigns.stage_buffer, { desc = "stage buffer" } },
		{ "R", gitsigns.reset_buffer, { desc = "reset buffer" } },
		{ "r", gitsigns.reset_hunk, { desc = "reset hunk" } },
		{ "p", gitsigns.preview_hunk, { desc = "preview hunk" } },
		{ "d", gitsigns.toggle_deleted, { nowait = true, desc = "toggle deleted" } },
		{ "b", gitsigns.blame_line, { desc = "blame" } },
		{ "B", gitsigns.toggle_current_line_blame, { desc = "toggle blame line" } },
		{
			"g",
			function()
				require("core.utils").ToggleLazygit()
			end,
			{ exit = true, nowait = true, desc = "Open Lazygit" },
		},
		{ "<Esc>", nil, { exit = true, nowait = true, desc = false } },
	},
})
local dap = require("dap")
local dap_hint = [[
_<F7>_: Step over  _<F8>_: Step into  _<F9>_: Step out  _h_: Hover          _f_: Frames  _s_: Scopes
_r_: Toggle REPL   _p_: Preview       _l_: Run last     _<F6>_: Continue    ^_q_: Quit
]]
Hydra({
	name = "Debug",
	hint = dap_hint,
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
		{ "<F6>", "<cmd>DapContinue<cr>", { exit = false, desc = "Continue" } },
		{ "<F7>", "<cmd>DapStepOver<cr>", { exit = false, desc = "Step over" } },
		{ "<F8>", "<cmd>DapStepInto<cr>", { exit = false, desc = "Step into" } },
		{ "<F9>", "<cmd>DapStepOut<cr>", { exit = false, desc = "Step out" } },
		{ "q", "<cmd>DapTerminate<cr>", { exit = true, desc = "Stop debugging" } },
		{ "r", "<cmd>DapToggleRepl<cr>", { exit = true, desc = "REPL" } },
		{ "l", dap.run_last, { exit = false, desc = "Run last" } },
		{ "h", require('dap.ui.widgets').hover, { exit = true, desc = "Hover" } },
		{ "p", require('dap.ui.widgets').preview, { exit = true, desc = "Preview" } },
		{
			"f",
			function()
				local widgets = require("dap.ui.widgets")
				widgets.centered_float(widgets.frames)
			end,
			{ exit = true, desc = "Frames" },
		},
		{
			"s",
			function()
				local widgets = require("dap.ui.widgets")
				widgets.centered_float(widgets.scopes)
			end,
			{ exit = true, desc = "Scopes" },
		},
		{ "<Esc>", nil, { exit = true, nowait = true, desc = false } },
	},
})
