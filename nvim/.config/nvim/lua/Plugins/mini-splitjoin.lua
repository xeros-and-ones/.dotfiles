local M = {
  "echasnovski/mini.splitjoin",
  version = false,
  enabled = true,
  lazy = false,
}

function M.config()
  require("mini.splitjoin").setup {
    mappings = {
      toggle = "gs",
      split = "",
      join = "",
    },
  }
end

return M
