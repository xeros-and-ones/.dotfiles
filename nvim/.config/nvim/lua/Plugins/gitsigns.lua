local M = {
    "lewis6991/gitsigns.nvim",
    commit = "2272cf9f0c092e908f892f5b075e6cc2a8d3d07d",
    enabled = true,
    event = { "BufReadPost", "BufNewFile" },
}

function M.config()
    require("gitsigns").setup()
    local gitsigns = require "gitsigns"
    local Hydra = require "hydra"
    local cmd = require("hydra.keymap-util").cmd
    local hint = [[
	_]_: next hunk    _s_: stage hunk        _d_: show deleted   _b_: blame line
	_[_: prev hunk    _S_: stage buffer      _p_: preview hunk   _B_: toggle blame line
	_D_: diff view    _R_: reset buffer      _u_: undo last stage^ ^
	]]
    Hydra {
        name = "Git",
        hint = hint,
        config = {
            color = "pink",
            invoke_on_body = true,
            hint = {
                position = "bottom",
                border = "rounded",
            },
            on_enter = function()
                vim.cmd "mkview"
                vim.cmd "silent! %foldopen!"
                vim.bo.modifiable = false
                gitsigns.toggle_linehl(true)
                gitsigns.toggle_signs(true)
                gitsigns.toggle_deleted(false)
            end,
            on_exit = function()
                local cursor_pos = vim.api.nvim_win_get_cursor(0)
                vim.cmd "loadview"
                vim.api.nvim_win_set_cursor(0, cursor_pos)
                vim.cmd "normal zv"
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
            { "D",     gitsigns.diffthis,                  { exit = false, silent = true, desc = "diff view" } },
            { "s",     cmd "Gitsigns stage_hunk",          { silent = true, desc = "stage hunk" } },
            { "u",     gitsigns.undo_stage_hunk,           { desc = "undo last stage" } },
            { "S",     gitsigns.stage_buffer,              { desc = "stage buffer" } },
            { "R",     gitsigns.reset_buffer,              { desc = "reset buffer" } },
            { "p",     gitsigns.preview_hunk,              { desc = "preview hunk" } },
            { "d",     gitsigns.toggle_deleted,            { nowait = true, desc = "toggle deleted" } },
            { "b",     gitsigns.blame_line,                { desc = "blame" } },
            { "B",     gitsigns.toggle_current_line_blame, { desc = "toggle blame line" } },
            { "<Esc>", nil,                                { exit = true, nowait = true, desc = false } },
        },
    }
end

return M
