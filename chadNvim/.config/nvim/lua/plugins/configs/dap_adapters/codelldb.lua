-- rust and codelldb setup
local dap = require "dap"
local codelldb_root = vim.fn.stdpath "data" .. "/mason/packages/codelldb/extension/"
local codelldb_path = codelldb_root .. "adapter/codelldb"
local liblldb_path = codelldb_root .. "lldb/lib/liblldb.lib"

dap.adapters.codelldb = {
  type = "server",
  port = "${port}",
  executable = {
    command = codelldb_path,
    args = { "--port", "${port}" },
  },
}

dap.configurations.rust = {
  {
    name = "Launch file",
    type = "codelldb",
    request = "launch",
    program = function()
      return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
    end,
    cwd = "${workspaceFolder}",
    stopOnEntry = false,
    showDisassembly = "never",
  },
}

dap.configurations.c = dap.configurations.rust
