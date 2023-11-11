local dap = require("dap")

dap.adapters.python = {
	type = "executable",
	command = vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python",
	args = { "-m", "debugpy.adapter" },
}

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
			local cwd = vim.fn.getcwd()
			if vim.fn.glob(cwd .. "/poetry.lock") then
				local venv = vim.fn.trim(vim.fn.system("poetry env info -p"))
				return venv .. "/bin" .. "/python"
			end
			if vim.fn.executable(cwd .. "/venv/bin/python") == 1 then
				return cwd .. "/venv/bin/python"
			elseif vim.fn.executable(cwd .. "/.venv/bin/python") == 1 then
				return cwd .. "/.venv/bin/python"
			elseif vim.fn.exists("$VIRTUAL_ENV") then
				return get_python_path()
			else
				return "/usr/bin/python"
			end
		end,
	},
}
