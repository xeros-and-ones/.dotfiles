local M = {
  "sindrets/diffview.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  enabled = true,
  commit = "a111d19ccceac6530448d329c63f998f77b5626e",
}

M.keys = {
  { "<C-g>", "<CMD>DiffviewOpen<CR>", mode = { "n", "i", "v" } },
}
M.opts = {
  enhanced_diff_hl = true,
  keymaps = {
    view = {
      ["<C-g>"] = "<CMD>DiffviewClose<CR>",
      ["c"] = "<CMD>DiffviewClose|Neogit commit<CR>",
    },
    file_panel = {
      ["<C-g>"] = "<CMD>DiffviewClose<CR>",
      ["c"] = "<CMD>DiffviewClose|Neogit commit<CR>",
    },
  },
}

return M
