local M = {
    "tpope/vim-dadbod",
    commit = "95fd22469507e86b78aa55d868c14108adee2881",
    enabled = true,
    dependencies = {
        "kristijanhusak/vim-dadbod-ui",
        "kristijanhusak/vim-dadbod-completion"
    },
    cmd = { "DBUIToggle", "DBUI", "DBUIAddConnection", "DBUIFindBuffer", "DBUIRenameBuffer", "DBUILastQueryInfo" },
}
M.keys = {
    { "<leader>Dt", "<Cmd>DBUIToggle<Cr>",        desc = "Toggle UI" },
    { "<leader>Df", "<Cmd>DBUIFindBuffer<Cr>",    desc = "Find buffer" },
    { "<leader>Dr", "<Cmd>DBUIRenameBuffer<Cr>",  desc = "Rename buffer" },
    { "<leader>Dq", "<Cmd>DBUILastQueryInfo<Cr>", desc = "Last query info" },
}


local function db_completion()
    require("cmp").setup.buffer { sources = { { name = "vim-dadbod-completion" } } }
end


function M.config()
    vim.g.db_ui_save_location = vim.fn.stdpath "config" .. require("plenary.path").path.sep .. "db_ui"


    vim.api.nvim_create_autocmd("FileType", {
        pattern = {
            "sql",
        },
        command = [[setlocal omnifunc=vim_dadbod_completion#omni]],
    })

    vim.api.nvim_create_autocmd("FileType", {
        pattern = {
            "sql",
            "mysql",
            "plsql",
        },
        callback = function()
            vim.schedule(db_completion)
        end,
    })
end

return M
