local dap = require "dap"
vim.fn.sign_define("DapBreakpoint", { text = "🛑", texthl = "DiagnosticSignError", linehl = "", numhl = "" })

local dapui = require "dapui"
dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end

dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end

dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end

local Hydra = require "hydra"
local cmd = require("hydra.keymap-util").cmd
local hint = [[
              _<cr>_: Continue      _b_: Breakpoint      _B_: Cond Breakpoint     _f_: Frames
              _<f6>_: Step over     _r_: REPL            _p_: Preview             _s_: Scopes
              _<f7>_: Step into     _l_: Run last        _h_: Hover
              _<f8>_: Step out                                                ^_q_: Quit
            ]]
Hydra {
  name = "Debug",
  hint = hint,
  config = {
    color = "pink",
    invoke_on_body = true,
    hint = {
      position = "bottom",
      border = "rounded",
    },
  },
  mode = { "n", "x" },
  body = "<leader>d",
  heads = {
    { "<f6>", cmd "lua require('dap').step_over()", { exit = false, desc = "Step over" } },
    { "<f7>", cmd "lua require('dap').step_into()", { exit = false, desc = "Step into" } },
    { "<f8>", cmd "lua require('dap').step_out()", { exit = false, desc = "Step out" } },
    { "b", cmd "lua require('dap').toggle_breakpoint()", { exit = false, desc = "Toggle breakpoint" } },
    {
      "B",
      cmd "lua require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: '))",
      { exit = false, desc = "Breakpoint Condition" },
    },
    { "<cr>", cmd "lua require('dap').continue()", { exit = false, desc = "Continue" } },
    { "r", cmd "lua require('dap').repl.toggle()", { exit = false, desc = "REPL" } },
    { "q", cmd "lua require('dap').terminate()", { exit = true, desc = "Stop debugging" } },
    { "l", cmd "lua require('dap').run_last()", { exit = true, desc = "Run last" } },
    { "h", cmd "lua require('dap.ui.widgets').hover()", { exit = true, desc = "Hover" } },
    { "p", cmd "lua require('dap.ui.widgets').preview()", { exit = true, desc = "Preview" } },
    {
      "f",
      function()
        local widgets = require "dap.ui.widgets"
        widgets.centered_float(widgets.frames)
      end,
      { exit = true, desc = "Frames" },
    },
    {
      "s",
      function()
        local widgets = require "dap.ui.widgets"
        widgets.centered_float(widgets.scopes)
      end,
      { exit = true, desc = "Scopes" },
    },
    { "<Esc>", nil, { exit = true, nowait = true, desc = false } },
  },
}
-- NOTE: Make sure to install the needed files/exectubles through mason
require "custom.configs.dap_adapters.cpptools"
require "custom.configs.dap_adapters.debugpy"
require "custom.configs.dap_adapters.go-debug-adapter"
require "custom.configs.dap_adapters.codelldb"
