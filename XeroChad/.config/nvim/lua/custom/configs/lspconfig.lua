dofile(vim.g.base46_cache .. "lsp")

local utils = require("core.utils")
local cmp_nvim_lsp = require("cmp_nvim_lsp")

---------------------------------------------------------
local signs = { Error = "", Warn = "", Hint = "󰌵", Info = "" }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.diagnostic.config({
	-- Enable virtual text
	virtual_text = false,
	-- show signs
	signs = {
		active = signs,
	},
	update_in_insert = true,
	underline = true,
	severity_sort = true,
	float = {
		focusable = false,
		style = "minimal",
		border = "rounded",
		source = "always",
		header = "",
		prefix = "",
	},
})

---------------------------------------------------------
require("lspconfig.ui.windows").default_options.border = "rounded"

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = "rounded",
})
-- vim.lsp.handlers["window/logMessage"] = function(_, content, _)
-- 	if content.type == 3 then
-- 		if content.message:find("pythonPath") then
-- 			vim.notify(content.message)
-- 		end
-- 	end
-- end
---------------------------------------------------------
local function lsp_mappings(buffer)
	vim.keymap.set("n", "gk", "<cmd>Lspsaga show_line_diagnostics<cr>", { desc = "Line Diagnostic", buffer = buffer })
	vim.keymap.set(
		"n",
		"gm",
		"<cmd>Telescope lsp_implementations<cr>",
		{ desc = "LSP implementation", buffer = buffer }
	)
	vim.keymap.set("n", "]d", "<cmd>Lspsaga diagnostic_jump_next<cr>", { desc = "Next Diagnostic", buffer = buffer })
	vim.keymap.set("n", "[d", "<cmd>Lspsaga diagnostic_jump_prev<cr>", { desc = "Prev Diagnostic", buffer = buffer })
	vim.keymap.set(
		{ "n", "v" },
		"<leader>la",
		"<cmd>Lspsaga code_action<cr>",
		{ desc = "Code Action", buffer = buffer }
	)
	vim.keymap.set("n", "<leader>ld", "<cmd>Lspsaga goto_definition<cr>", { desc = "Goto_Definition", buffer = buffer })
	vim.keymap.set("n", "<leader>lO", "<cmd>Lspsaga outline<cr>", { desc = "Code Outline", buffer = buffer })
	vim.keymap.set("n", "<leader>li", "<cmd>Lspsaga incoming_calls<cr>", { desc = "Incoming Calls", buffer = buffer })
	vim.keymap.set("n", "<leader>lo", "<cmd>Lspsaga outgoing_calls<cr>", { desc = "Outgoing Calls" })
	vim.keymap.set("n", "<leader>lr", "<cmd>Lspsaga rename<cr>", { desc = "Rename", buffer = buffer })
	vim.keymap.set(
		"n",
		"<leader>lR",
		"<cmd>Lspsaga rename ++project<cr>",
		{ desc = "Rename [Project Wide]", buffer = buffer }
	)
	vim.keymap.set("n", "<leader>lz", "<cmd>LspRestart<cr>", { desc = "Restart LSP", buffer = buffer })

	vim.keymap.set(
		"n",
		"<leader>lF",
		"<cmd>Lspsaga finder tyd+ref+imp+def ++normal<cr>",
		{ desc = "LspSaga Finder", buffer = buffer }
	)
	vim.keymap.set(
		"n",
		"<leader>lq",
		"<cmd>TroubleToggle quickfix<cr>",
		{ desc = "Quickfix [Trouble]", buffer = buffer }
	)
	vim.keymap.set("n", "<leader>lh", function()
		vim.lsp.inlay_hint(0, nil)
	end, { desc = "Inlay Hint" })
	vim.keymap.set("n", "<leader>lp", "<cmd>Lspsaga peek_definition<cr>", { desc = "Peek_Definition", buffer = buffer })
	vim.keymap.set(
		"n",
		"<leader>lD",
		"<cmd>TroubleToggle lsp_definitions<cr>",
		{ desc = "Definition [Trouble]", buffer = buffer }
	)
	vim.keymap.set(
		"n",
		"<leader>lf",
		"<cmd>TroubleToggle lsp_references<cr>",
		{ desc = "Find references [Trouble]", buffer = buffer }
	)
	vim.keymap.set(
		"n",
		"<leader>lt",
		"<cmd>TroubleToggle lsp_type_definitions<cr>",
		{ desc = "Type Definition [Trouble]", buffer = buffer }
	)
	vim.keymap.set(
		"n",
		"<leader>lx",
		"<cmd>TroubleToggle document_diagnostics<cr>",
		{ desc = "Buffer Diagnostics", buffer = buffer }
	)
	vim.keymap.set(
		"n",
		"<leader>lw",
		"<cmd>TroubleToggle workspace_diagnostics<cr>",
		{ desc = "Workspace Diagnostics", buffer = buffer }
	)
	vim.keymap.set("n", "<leader>ls", function()
		vim.lsp.buf.signature_help()
	end, { desc = "LSP signature help", buffer = buffer })
end

---------------------------------------------------------
local on_attach = function(client, buffer)
	lsp_mappings(buffer)

	vim.bo[buffer].formatexpr = "" --  yikes
	local caps = client.server_capabilities

	if not utils.load_config().ui.lsp_semantic_tokens and client.supports_method("textDocument/semanticTokens") then
		client.server_capabilities.semanticTokensProvider = nil
	end

	-- if caps.documentHighlightProvider then
	-- 	local group = vim.api.nvim_create_augroup("DocumentHighlight", {})
	-- 	vim.api.nvim_create_autocmd("CursorHold", {
	-- 		group = group,
	-- 		buffer = 0,
	-- 		callback = vim.lsp.buf.document_highlight,
	-- 	})
	-- 	vim.api.nvim_create_autocmd("CursorMoved", {
	-- 		group = group,
	-- 		buffer = 0,
	-- 		callback = vim.lsp.buf.clear_references,
	-- 	})
	-- end

	if caps.documentFormattingProvider then
		local group = vim.api.nvim_create_augroup("Formatting", {})
		vim.api.nvim_create_autocmd("BufWritePre", {
			group = group,
			buffer = 0,
			callback = function()
				if vim.g.format_on_save then
					require("luasnip").session.current_nodes[vim.api.nvim_get_current_buf()] = nil
					vim.lsp.buf.format({
						timeout_ms = 3000,
						filter = function(c)
							return c.name == "null-ls"
						end,
					})
				end
			end,
		})
	end
end
-- --------------------------------------------------------
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = cmp_nvim_lsp.default_capabilities()
capabilities.offsetEncoding = { "utf-16" }
capabilities.textDocument.completion.completionItem = {
	documentationFormat = { "markdown", "plaintext" },
	snippetSupport = true,
	preselectSupport = true,
	insertReplaceSupport = true,
	labelDetailsSupport = true,
	deprecatedSupport = true,
	commitCharactersSupport = true,
	tagSupport = { valueSet = { 1 } },
	resolveSupport = {
		properties = {
			"documentation",
			"detail",
			"additionalTextEdits",
		},
	},
}
capabilities.textDocument.foldingRange = {
	dynamicRegistration = false,
	lineFoldingOnly = true,
}
---------------------------------------------------------
require("custom.configs.lsp_servers.custom")
local path = require("mason-core.path")

require("lspconfig").pylance.setup({
	capabilities = capabilities,
	on_attach = on_attach,

	on_init = function(client)
		client.config.settings.python.pythonPath = (function(workspace)
			if not workspace then
				return vim.fn.exepath("python3") or vim.fn.exepath("python") or "python"
			end
			-- local poetry_lock_path = vim.fs.joinpath(workspace, "poetry.lock")
			local venv_path = os.getenv("VIRTUAL_ENV")
			local py_path = nil
			-- decide which python executable to use for mypy
			if venv_path ~= nil then
				py_path = venv_path .. "/bin/python3"
			else
				py_path = vim.fn.exepath("python3") or vim.fn.exepath("python") or "python"
			end
			return py_path
		end)(client.config.root_dir)
	end,
	before_init = function(_, config)
		config.settings.python.analysis.stubPath = path.concat({
			vim.fn.stdpath("data"),
			"lazy",
			"python-type-stubs",
		})
	end,
})
---------------------------------------------------------
local mason_lspconfig = require("mason-lspconfig")
mason_lspconfig.setup()

local disabled_servers = {
	"pylsp",
}

mason_lspconfig.setup_handlers({
	function(server_name)
		for _, name in pairs(disabled_servers) do
			if name == server_name then
				return
			end
		end
		local opts = {
			on_attach = on_attach,
			capabilities = capabilities,
		}

		local require_ok, server = pcall(require, "custom.configs.lsp_servers." .. server_name)
		if require_ok then
			opts = vim.tbl_deep_extend("force", server, opts)
		end

		require("lspconfig")[server_name].setup(opts)
	end,
})
