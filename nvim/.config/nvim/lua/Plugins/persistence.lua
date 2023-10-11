local M = {
  "folke/persistence.nvim",
  commit = "4b8051c01f696d8849a5cb8afa9767be8db16e40",
  event = "BufReadPre", -- this will only start session saving when an actual file was opened
}

M.opts = {
  dir = vim.fn.expand(vim.fn.stdpath "state" .. "/sessions/"), -- directory where session files are saved
  options = { "buffers", "curdir", "folds", "tabpages", "winsize", "globals" }, -- sessionoptions used for saving
  pre_save = nil, -- a function to call before saving the session
}

return M
