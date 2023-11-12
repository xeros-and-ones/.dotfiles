local venv_path = os.getenv("VIRTUAL_ENV")
local py_path = nil
-- decide which python executable to use for mypy
if venv_path ~= nil then
	py_path = venv_path .. "/bin/python3"
else
	py_path = vim.g.python3_host_prog
end
return {
	single_file_support = true,
	settings = {
		pylsp = {
			plugins = {
				configurationSources = { "pycodestyle" },
				-- formatter options
				black = { enabled = false, line_length = 100, cache_config = true },
				autopep8 = { enabled = false },
				yapf = { enabled = false },
				-- linter options
				pylint = { enabled = false, executable = "pylint" },
				ruff = {
					enabled = true,
					select = { "E4", "E7", "E9", "F" },
					format = "I",
					lineLength = 100,
				},
				pyflakes = { enabled = false },
				pycodestyle = { enabled = false },
				pydocstyle = { enabled = false },
				mccabe = { enabled = false },
				-- type checker
				pylsp_mypy = {
					enabled = true,
					overrides = { "--python-executable", py_path, true },
					report_progress = true,
					live_mode = false,
				},
				-- auto-completion options
				jedi_completion = {
					enabled = true,
					fuzzy = true,
					include_params = false,
				},
				-- import sorting
				pyls_isort = { enabled = false },
				rope_completion = { enabled = true },
				rope_autoimport = { enabled = true },
			},
		},
	},
}
