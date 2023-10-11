local M = {
  "nvim-pack/nvim-spectre",
  cmd = "Spectre",
}
M.opts = { open_cmd = "noswapfile vnew" }
M.keys = {
  {
    "<leader>st",
    function()
      require("spectre").toggle()
    end,
    desc = "Replace in files (Spectre)",
  },
  {
    "<leader>sc",
    '<cmd>lua require("spectre").open_file_search({select_word=true})<CR>',
    desc = "Search on current file",
  },
}

return M
