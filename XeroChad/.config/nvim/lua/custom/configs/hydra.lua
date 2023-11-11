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

local dap_hint = [[
 _<f9>_: step back     _b_: Breakpoint      _h_: Hover         _f_: Frames
 _<f6>_: Step over     _r_: Toggle REPL     _p_: Preview       _s_: Scopes
 _<f7>_: Step into     _l_: Run last
 _<f8>_: Step out               _<cr>_: Continue            ^_q_: Quit
]]
Hydra({
	name = "Debug",
	hint = dap_hint,
	config = {
		color = "red",
		invoke_on_body = true,
		hint = {
			position = "bottom",
			border = "rounded",
		},
	},
	mode = { "n", "x" },
	body = "<leader>d",
	heads = {
		{ "<cr>", cmd("lua require('dap').continue()"), { exit = false, desc = "Continue" } },
		{ "<f6>", cmd("lua require('dap').step_over()"), { exit = false, desc = "Step over" } },
		{ "<f7>", cmd("lua require('dap').step_into()"), { exit = false, desc = "Step into" } },
		{ "<f8>", cmd("lua require('dap').step_out()"), { exit = false, desc = "Step out" } },
		{ "<f9>", cmd("lua require('dap').step_back()"), { exit = false, desc = "Step out" } },
		{ "q", cmd("lua require('dap').terminate()"), { exit = true, desc = "Stop debugging" } },
		{ "b", cmd("lua require('dap').toggle_breakpoint()"), { exit = false, desc = "Toggle breakpoint" } },
		{ "r", cmd("lua require('dap').repl.toggle()"), { exit = true, desc = "REPL" } },
		{ "l", cmd("lua require('dap').run_last()"), { exit = true, desc = "Run last" } },
		{ "h", cmd("lua require('dap.ui.widgets').hover()"), { exit = true, desc = "Hover" } },
		{ "p", cmd("lua require('dap.ui.widgets').preview()"), { exit = true, desc = "Preview" } },
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
