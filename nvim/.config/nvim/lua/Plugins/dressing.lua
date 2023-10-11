local M = {
  "stevearc/dressing.nvim",
  commit = "8f4d62b7817455896a3c73cab642002072c114bc",
  enabled = true,
  event = "VeryLazy",
}

function M.config()
  require("dressing").setup {
    input = {
      enabled = false,
    },
  }
end

return M
