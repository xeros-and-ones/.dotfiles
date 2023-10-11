local M = {
  "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
  commit = "9e3f99fbbd28aaec80dc0158c43be8cca8dd5017",
  event = "BufRead",
}

M.config = function()
  require("lsp_lines").setup()
end

return M
