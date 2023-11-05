local M = {
    "lewis6991/gitsigns.nvim",
    commit = "2272cf9f0c092e908f892f5b075e6cc2a8d3d07d",
    enabled = true,
    event = { "BufReadPost", "BufNewFile" },
    dependencies = {
        {
            "sindrets/diffview.nvim",
            event = "VeryLazy",
            config = true,
        },
    },
}
M.init = function()
    -- load gitsigns only when a git file is opened
    vim.api.nvim_create_autocmd({ "BufRead" }, {
        group = vim.api.nvim_create_augroup("GitSignsLazyLoad", { clear = true }),
        callback = function()
            vim.fn.system("git -C " .. '"' .. vim.fn.expand "%:p:h" .. '"' .. " rev-parse")
            if vim.v.shell_error == 0 then
                vim.api.nvim_del_augroup_by_name "GitSignsLazyLoad"
                vim.schedule(function()
                    require("lazy").load { plugins = { "gitsigns.nvim" } }
                end)
            end
        end,
    })
end
M.config = function()
    require("gitsigns").setup {
        signs = {
            add = { text = "│" },
            change = { text = "│" },
            delete = { text = "_" },
            topdelete = { text = "‾" },
            changedelete = { text = "~" },
            untracked = { text = "┆" },
        },
    }
    local gitsigns = require "gitsigns"
    local Hydra = require "hydra"
    local cmd = require("hydra.keymap-util").cmd
    local hint = [[
	_]_: next hunk    _s_: stage hunk       _d_: show deleted     _b_: blame line
	_[_: prev hunk    _S_: stage buffer     _p_: preview hunk     _B_: toggle blame line
	_D_: diff view    _R_: reset buffer     _r_: reset hunk       _u_: undo last stage
    _g_: Open LazyGit ^ ^
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
            { "D", gitsigns.diffthis, { exit = false, silent = true, desc = "diff view" } },
            { "s", cmd "Gitsigns stage_hunk", { silent = true, desc = "stage hunk" } },
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
                    require("Core.utils").ToggleLazygit()
                end,
                { exit = true, nowait = true, desc = "Open Lazygit" },
            },
            { "<Esc>", nil, { exit = true, nowait = true, desc = false } },
        },
    }
end

return M
