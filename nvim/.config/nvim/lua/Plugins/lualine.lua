local M = {
    "nvim-lualine/lualine.nvim",
    commit = "45e27ca739c7be6c49e5496d14fcf45a303c3a63",
    event = { "VimEnter", "InsertEnter", "BufReadPre", "BufAdd", "BufNew", "BufReadPost" },
}

function M.config()
    local status_ok, lualine = pcall(require, "lualine")
    if not status_ok then
        return
    end

    local hide_in_width = function()
        return vim.fn.winwidth(0) > 80
    end

    local diagnostics = {
        "diagnostics",
        sources = { "nvim_diagnostic", "nvim_lsp" },
        sections = { "error", "warn", "info", "hint" },
        symbols = { error = " ", warn = " ", info = " ", hint = "󰌵" },
        colored = true,
        always_visible = false,
        update_in_insert = true,
    }

    -- cool function for progress
    -- local progress = {
    -- 	cond = hide_in_width,
    -- 	function()
    -- 		local current_line = vim.fn.line(".")
    -- 		local total_lines = vim.fn.line("$")
    -- 		local chars = { "__", "▁▁", "▂▂", "▃▃", "▄▄", "▅▅", "▆▆", "▇▇", "██" }
    -- 		local line_ratio = current_line / total_lines
    -- 		local index = math.ceil(line_ratio * #chars)
    -- 		return chars[index]
    -- 	end,
    -- }
    local treesitter = {
        cond = hide_in_width,
        function()
            local ts_avail, ts = pcall(require, "nvim-treesitter.parsers")
            if not (ts_avail and ts.has_parser()) then
                return " No TS"
            else
                return ""
            end
        end,
    }
    --local spaces = function()
    --return "spaces: " .. vim.api.nvim_buf_get_option(0, "shiftwidth")
    --end
    lualine.setup {
        options = {
            icons_enabled = true,
            theme = "powerline_gruvbox",
            -- component_separators = { left = "┃", right = "┃" },
            section_separators = { left = "", right = "" },
            disabled_filetypes = {
                "alpha",
                -- "neo-tree",
                -- "NvimTree",
                "dashboard",
                "Outline",
                statusline = {},
                winbar = {},
            },
            ignore_focus = {},
            always_divide_middle = true,
            globalstatus = false,
            refresh = {
                statusline = 1000,
                tabline = 1000,
                winbar = 1000,
            },
        },

        sections = {
            lualine_a = {
                {
                    function()
                        if require("hydra.statusline").is_active() then
                            return require("hydra.statusline").get_name()
                        else
                            return require("lualine.utils.mode").get_mode()
                        end
                    end,
                    separator = { right = "" },
                },
            },
            lualine_b = {
                {
                    "branch",
                    icons_enabled = true,
                    -- icon = "",
                    icon = { "", color = { fg = "#b8bb26" } },
                },
                {
                    "diff",
                    colored = true,
                    symbols = { added = " ", modified = " ", removed = " " }, -- changes diff symbols
                    cond = hide_in_width,
                },
            },
            lualine_c = {
                { "filetype", colored = true, icon_only = false, cond = hide_in_width },
                {
                    "filename",
                    file_status = true,
                    path = 1,
                    shorting_target = 105,
                    symbols = {
                        modified = "[+]",
                        readonly = "[-]",
                        unnamed = "No Name",
                        newfile = "New",
                    },
                },
            },
            lualine_x = {
                {
                    require("noice").api.statusline.mode.get,
                    cond = require("noice").api.statusline.mode.has,
                    color = { fg = "#ff2800", bg = "#000000" },
                    separator = { left = "", right = "" },
                },
                {
                    function()
                        return "  "
                    end,
                    cond = function()
                        local tasks = require("overseer").list_tasks()
                        if vim.tbl_isempty(tasks) then
                            return false
                        else
                            return true
                        end
                    end,
                    color = { fg = "#000000", bg = "#5a5ad8" },
                    separator = { left = "" },
                },
                {
                    "overseer",
                    on_click = function()
                        vim.cmd "OverseerToggle"
                    end,
                    color = { fg = "#000000", bg = "#5a5ad8" },
                    separator = { right = "" },
                },
            },
            lualine_y = {
                diagnostics,
                treesitter,
                {
                    function()
                        local clients = {}
                        local buf = vim.api.nvim_get_current_buf()

                        -- Iterate through all the clients for the current buffer
                        for _, client in pairs(vim.lsp.get_active_clients { bufnr = buf }) do
                            table.insert(clients, client.name)
                        end
                        if #clients == 0 then
                            return "No Active Lsp"
                        else
                            return table.concat(clients, ", ")
                        end
                    end,
                    cond = hide_in_width,
                    icon = " ",
                    color = { gui = "bold" },
                    on_click = function()
                        vim.cmd "LspInfo"
                    end,
                },
            },
            lualine_z = {
                {
                    function()
                        return ""
                    end,
                    separator = { left = "" },
                },
                "progress",
                {
                    "location", --'%l:%c ‖ %p%%',
                    cond = hide_in_width,
                    separator = { left = "" },
                },
            },
        },
        inactive_sections = {
            lualine_a = { "filename" },
            lualine_b = {},
            lualine_c = {},
            lualine_x = {},
            lualine_y = {},
            lualine_z = { "location" },
        },
        -- tabline = {},
        winbar = {
            lualine_a = {},
            lualine_b = {},
            lualine_c = {},
            lualine_x = {},
            lualine_y = {},
            lualine_z = {},
        },
        -- inactive_winbar = {},
        extensions = {
            "toggleterm",
            "aerial",
            "overseer",
            "neo-tree",
            "trouble",
        },
    }
end

return M
