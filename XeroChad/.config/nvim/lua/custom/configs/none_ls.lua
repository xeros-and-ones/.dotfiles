local formatting = require("null-ls").builtins.formatting
local diagnostics = require("null-ls").builtins.diagnostics
local code_actions = require("null-ls").builtins.code_actions
require("null-ls").setup({
	border = "rounded",

	sources = {
		--formatting
		formatting.djlint.with({
			filetypes = {
				"django",
				"jinja",
				"htmldjango",
			},
		}),
		formatting.asmfmt,
		formatting.markdownlint,
		formatting.beautysh,
		formatting.rustfmt,
		formatting.stylua,
		formatting.cmake_format.with({
			cmd = "cmake-format",
		}),
		formatting.black.with({
			extra_args = function(_)
				return {
					"--fast",
					"--quiet",
				}
			end,
		}),
		formatting.isort,
		formatting.csharpier,
		formatting.clang_format.with({
			filetypes = {
				"c",
				"cpp",
			},
		}),
		formatting.gofumpt,
		formatting.prettier.with({
			filetypes = {
				"angular",
				"css",
				"flow",
				"graphql",
				"html",
				"json",
				"javascriptreact",
				"javascript",
				"less",
				"scss",
				"typescript",
				"typescriptreact",
				"vue",
				"yaml",
			},
			extra_filetypes = { "toml" },
			extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" },
		}),
		--------------------------------------------------------------------------------------
		--diagnostics
		-- diagnostics.markdownlint,
		diagnostics.djlint,
		diagnostics.shellcheck,
		diagnostics.jsonlint,
		diagnostics.zsh,
	},
})
