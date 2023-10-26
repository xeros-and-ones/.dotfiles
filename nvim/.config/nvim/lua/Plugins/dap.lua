local M = {
    "mfussenegger/nvim-dap",
    enabled = true,
    dependencies =
    {
        "mfussenegger/nvim-dap-python",
        {
            "theHamsta/nvim-dap-virtual-text",
            enabled = true,
            config = function()
                require("nvim-dap-virtual-text").setup({
                    highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
                    highlight_new_as_changed = false,   -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
                    show_stop_reason = true,            -- show stop reason when stopped for exceptions
                    only_first_definition = true,       -- only show virtual text at first definition (if there are multiple)
                    all_references = true,              -- show virtual text on all all references of the variable (not only definitions)

                    -- experimental features:
                    virt_text_pos = "eol", -- position of virtual text, see `:h nvim_buf_set_extmark()`
                    all_frames = false,    -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
                    virt_lines = false,    -- show virtual lines instead of virtual text (will flicker!)
                })
            end

        },
        {
            "rcarriga/nvim-dap-ui",
            enabled = true,
            config = function()
                local dap, dapui = require("dap"), require("dapui")
                dapui.setup()

                dap.listeners.after.event_initialized["dapui_config"] = function()
                    dapui.open()
                end
                dap.listeners.before.event_terminated["dapui_config"] = function()
                    dapui.close()
                end
                dap.listeners.before.event_exited["dapui_config"] = function()
                    dapui.close()
                end
            end

        }
    },
    event = "VeryLazy",
}

-- TODO: fix cleanup the dap config

function M.config()
    -- python debugpy setup
    local path_debugpy = vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python"
    require("dap-python").setup(path_debugpy)

    -- rust and codelldb setup
    local dap = require("dap")
    local codelldb_root = vim.fn.stdpath("data") .. "/mason/packages/codelldb/extension/"
    local codelldb_path = codelldb_root .. "adapter/codelldb.exe"
    local liblldb_path = codelldb_root .. "lldb/lib/liblldb.lib"

    dap.adapters.codelldb = {
        type = "server",
        port = "${port}",
        executable = {
            command = codelldb_path,
            args = { "--port", "${port}" },
            -- On windows you may have to uncomment this:
            -- detached = false,
        },
    }
    dap.configurations.rust = {
        {
            name = "Debug",
            type = "codelldb",
            request = "launch",
            program = function()
                vim.notify("Compiling a debug build for debugging. This might take some time...")
                vim.fn.jobstart("cargo build")

                return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/target/debug/", "file")
            end,
            cwd = "${workspaceFolder}",
            stopOnEntry = false,
            showDisassembly = "never",
        },
    }

    -- set debugger signs
    vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DapBreakpoint", linehl = "", numhl = "" })
    vim.fn.sign_define("DapBreakpointCondition", { text = "", texthl = "DapBreakpoint", linehl = "", numhl = "" })
    vim.fn.sign_define("DapBreakpointRejected", { text = "", texthl = "DapBreakpoint", linehl = "", numhl = "" })
    vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DapLogPoint", linehl = "", numhl = "" })
    vim.fn.sign_define("DapStopped", { text = "", texthl = "DapStopped", linehl = "", numhl = "" })
end

return M
