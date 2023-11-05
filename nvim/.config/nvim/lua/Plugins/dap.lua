local M = {
    "rcarriga/nvim-dap-ui",

    dependencies = {
        {
            "mfussenegger/nvim-dap",
            config = function()
                -- NOTE: Check out this for guide
                -- https://github.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation
                local dap = require "dap"
                vim.fn.sign_define(
                    "DapBreakpoint",
                    { text = "🛑", texthl = "DiagnosticSignError", linehl = "", numhl = "" }
                )

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

                -- NOTE: Make sure to install the needed files/exectubles through mason
                -- require "plugins.dap.adapters.cpptools"
                require "Plugins.dap.adapters.debugpy"
                require "Plugins.dap.adapters.go-debug-adapter"
                require "Plugins.dap.adapters.codelldb"
            end,
        },
        {
            "theHamsta/nvim-dap-virtual-text",
            enabled = true,
            config = function()
                require("nvim-dap-virtual-text").setup {
                    highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
                    highlight_new_as_changed = false, -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
                    show_stop_reason = true, -- show stop reason when stopped for exceptions
                    only_first_definition = true, -- only show virtual text at first definition (if there are multiple)
                    all_references = true, -- show virtual text on all all references of the variable (not only definitions)

                    -- experimental features:
                    virt_text_pos = "eol", -- position of virtual text, see `:h nvim_buf_set_extmark()`
                    all_frames = false, -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
                    virt_lines = false, -- show virtual lines instead of virtual text (will flicker!)
                }
            end,
        },
    },
    opts = require "Plugins.dap.ui",
}

return M
