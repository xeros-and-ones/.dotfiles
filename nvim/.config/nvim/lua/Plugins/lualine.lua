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

    local lsp = {
        -- Lsp server name .
        function()
            local msg = "No Active Lsp"
            local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
            local clients = vim.lsp.get_active_clients()
            -- if next(clients) == nil then return msg end
            for _, client in ipairs(clients) do
                local filetypes = client.config.filetypes
                if client.name ~= "none-ls" then
                    if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
                        return client.name
                    end
                end
            end
            return msg
        end,
        icon = " ",
        color = { gui = "bold" },
        on_click = function()
            vim.cmd("LspInfo")
        end,
    }
    local treesitter = {
        cond = hide_in_width,
        function()
            local ts_avail, ts = pcall(require, "nvim-treesitter.parsers")
            return (ts_avail and ts.has_parser()) and " TS" or " No TS"
        end,
    }
    --local spaces = function()
    --return "spaces: " .. vim.api.nvim_buf_get_option(0, "shiftwidth")
    --end
    require("lualine").setup {
        options = {
            icons_enabled = true,
            theme = "powerline_custom",
            component_separators = { left = "┃", right = "┃" },
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
            lualine_a = { { "mode", separator = { right = "" } } },
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
                    symbols = { added = " ", modified = " ", removed = " " }, -- changes diff symbols
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
                        unnamed = "[No Name]",
                        newfile = "[New]",
                    },
                },
            },
            lualine_x = {
                {
                    require("noice").api.statusline.mode.get,
                    cond = require("noice").api.statusline.mode.has,
                    color = { fg = "#ff2800", bg = "#000000" },
                    separator = { left = "" }
                },
                {
                    function()
                        return "⚒"
                    end,
                    cond = function()
                        local tasks = require("overseer").list_tasks()
                        if vim.tbl_isempty(tasks) then
                            return false
                        else
                            return true
                        end
                    end,
                    separator = { left = "", right = "" },
                },
                {
                    "overseer",
                    on_click = function()
                        vim.cmd("OverseerToggle")
                    end,
                },
            },
            lualine_y = { diagnostics, treesitter, lsp },
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
            lualine_a = {},
            lualine_b = {},
            lualine_c = { "filename" },
            lualine_x = { "location" },
            lualine_y = {},
            lualine_z = {},
        },
        -- tabline = {},
        -- winbar = {
        --     lualine_a = {},
        --     lualine_b = {},
        --     lualine_c = {
        --     },
        --     lualine_x = {},
        --     lualine_y = {},
        --     lualine_z = {},
        -- },
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
