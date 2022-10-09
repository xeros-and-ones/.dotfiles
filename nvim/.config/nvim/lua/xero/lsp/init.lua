local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

require "xero.lsp.mason"
require("xero.lsp.handlers").setup()
require "xero.lsp.null-ls"
