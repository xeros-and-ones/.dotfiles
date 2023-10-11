local M = {
  "kristijanhusak/vim-dadbod-ui",
  commit = "95fd22469507e86b78aa55d868c14108adee2881",
  enabled = true,
  dependencies = { "tpope/vim-dadbod", "kristijanhusak/vim-dadbod-completion" },
  cmd = { "DB", "DBUI", "DBUIAddConnection" },
}
M.keys = {
  { "<leader>zd", "<cmd>DBUI<cr>", desc = "dadbod" },
}
vim.g.db_ui_show_help = 0
vim.g.db_ui_winwidth = 30

return M
