local M = {}

M.dap = function()
	local dap = require("dap")
	dap.defaults.fallback.terminal_win_cmd = "50vsplit new"

	require("custom.configs.dap_adapters.cpptools")
	require("custom.configs.dap_adapters.debugpy")
	require("custom.configs.dap_adapters.go-debug-adapter")
	require("custom.configs.dap_adapters.codelldb")
end

M.dapui = function()
	local dap = require("dap")
	local dapui = require("dapui")

	dapui.setup({
		icons = { expanded = "▾", collapsed = "▸" },
		mappings = {
			-- Use a table to apply multiple mappings
			expand = { "<CR>", "<2-LeftMouse>" },
			open = "o",
			remove = "d",
			edit = "e",
			repl = "r",
			toggle = "t",
		},
		layouts = {
			{
				elements = {
					{ id = "scopes", size = 0.25 },
					"breakpoints",
					"stacks",
					"watches",
				},
				size = 50,
				position = "left",
			},
			{
				elements = {
					"repl",
					"console",
				},
				size = 0.25,
				position = "bottom",
			},
		},
		floating = {
			max_height = nil,
			max_width = nil,
			border = "single",
			mappings = {
				close = { "q", "<Esc>" },
			},
		},
		windows = { indent = 1 },
	})

	vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DiagnosticSignError", linehl = "", numhl = "" })

	dap.listeners.after.event_initialized["dapui_config"] = function()
		dapui.open()
		vim.keymap.set({ "v", "n" }, "K", [[<Cmd>lua require("dapui").eval()<CR>]], { buffer = 0 })
	end
	dap.listeners.before.event_terminated["dapui_config"] = function()
		dapui.close()
	end
	dap.listeners.before.event_exited["dapui_config"] = function()
		dapui.close()
	end
end

return M
