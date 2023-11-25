local dap = require("dap")
local path = ""

if vim.fn.has("win32") == 1 then
	path = vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/Scripts/python"
else
	path = vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python"
end

dap.adapters.python = function(cb, config)
	if config.request == "attach" then
		local port = (config.connect or config).port
		local host = (config.connect or config).host or "127.0.0.1"
		cb({
			type = "server",
			port = assert(port, "`connect.port` is required for a python `attach` configuration"),
			host = host,
			options = {
				source_filetype = "python",
			},
		})
	else
		cb({
			type = "executable",
			command = path,
			args = { "-m", "debugpy.adapter" },
			options = {
				source_filetype = "python",
			},
		})
	end
end

local get_python_path = function()
	local venv_path = os.getenv("VIRTUAL_ENV")
	if venv_path then
		return venv_path .. "/bin/python"
	end
	return nil
end

dap.configurations.python = {
	{
		type = "python",
		request = "launch",
		name = "Launch file",
		program = "${file}",
		pythonPath = function()
			local venv_path = os.getenv("VIRTUAL_ENV")
			local py_path = nil
			-- decide which python executable to use for mypy
			if venv_path ~= nil then
				py_path = venv_path .. "/bin/python"
			else
				py_path = vim.fn.exepath("python3") or vim.fn.exepath("python") or "python"
				print(py_path)
			end
		end,
	},
}
