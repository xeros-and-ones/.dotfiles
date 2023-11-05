local M = {
    "williamboman/mason.nvim",
    cmd = { "Mason", "MasonInstall", "MasonInstallAll", "MasonUpdate" },
    dependencies = {
        "williamboman/mason-lspconfig.nvim",
    },
}

M.opts = {
    ui = {
        check_outdated_packages_on_open = false,
        border = "rounded",
        icons = {
            package_pending = " ",
            package_installed = "󰄳 ",
            package_uninstalled = " ",
        },

        keymaps = {
            toggle_server_expand = "<CR>",
            install_server = "i",
            update_server = "u",
            check_server_version = "c",
            update_all_servers = "U",
            check_outdated_servers = "C",
            uninstall_server = "X",
            cancel_installation = "<C-c>",
        },
    },

    max_concurrent_installers = 3,
    ensure_installed = {
        -- lsp ---------------------------------------
        "lua-language-server", --
        "taplo", --
        "vim-language-server",
        "css-lsp",
        "html-lsp",
        "python-lsp-server",
        -- "pyright",
        "typescript-language-server",
        "yaml-language-server",
        "json-lsp",
        "gopls",
        "sqlls",
        "clangd",

        -- debuggers ---------------------------------
        "codelldb",
        "cpptools",
        "debugpy",
        "go-debug-adapter",
        -- linters -----------------------------------
        "markdownlint",
        "djlint",
        "shellcheck",
        "jsonlint",
        -- "flake8",

        -- formatters -------------------------------
        "beautysh",
        "stylua",
        "gofumpt",
        "clang-format",
        "prettier",
    },
}

M.config = function()
    require("mason").setup(M.opts)

    -- custom nvchad cmd to install all mason binaries listed
    vim.api.nvim_create_user_command("MasonInstallAll", function()
        vim.cmd("MasonInstall " .. table.concat(M.opts.ensure_installed, " "))
    end, {})

    vim.g.mason_binaries_list = M.opts.ensure_installed
end

return M
