local M = {
    "folke/noice.nvim",
    enabled = true,
    dependencies = {
        "MunifTanjim/nui.nvim",
        "rcarriga/nvim-notify",
    },
    event = "VimEnter",
}

function M.config()
    require("noice").setup {
        lsp = {
            progress = {
                enabled = true,
                -- Lsp Progress is formatted using the builtins for lsp_progress. See config.format.builtin
                -- See the section on formatting for more details on how to customize.
                format = "lsp_progress",
                format_done = "lsp_progress_done",
                throttle = 1000 / 30, -- frequency to update lsp progress message
                view = "mini",
            },
            -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
            override = {
                ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                ["vim.lsp.util.stylize_markdown"] = true,
                ["cmp.entry.get_documentation"] = true,
            },
            hover = {
                enabled = false,
            },
            signature = {
                enabled = false,
                auto_open = {
                    enabled = true,
                    trigger = true, -- Automatically show signature help when typing a trigger character from the LSP
                    luasnip = true, -- Will open signature help when jumping to Luasnip insert nodes
                    throttle = 50, -- Debounce lsp signature help request by 50ms
                },
            },
        },
        notify = {
            enabled = true,
            require("notify").setup {
                stages = "fade_in_slide_out",
                background_colour = "#000000",
                opacity = 20,
                timeout = 3000,
            },
        },
        popupmenu = {
            enabled = true, -- enables the Noice popupmenu UI
            ---@type 'nui'|'cmp'
            backend = "cmp", -- backend to use to show regular cmdline completions
        },
        views = {
            mini = {
                win_options = {
                    winblend = 5,
                },
            },
        },
        routes = {
            {
                filter = {
                    event = "msg_show",
                    kind = "",
                    find = "written",
                },
                opts = { skip = true },
            },
            -- {
            --     filter = {
            --         event = "msg_show",
            --         kind = "search_count",
            --     },
            --     opts = { skip = true },
            -- },
        },
        presets = {
            bottom_search = false, -- use a classic bottom cmdline for search
            command_palette = true, -- position the cmdline and popupmenu together
            long_message_to_split = true, -- long messages will be sent to a split
            inc_rename = false, -- enables an input dialog for inc-rename.nvim
            lsp_doc_border = false, -- add a border to hover docs and signature help
        },
    }
    vim.keymap.set({ "n", "i", "s" }, "<c-d>", function()
        if not require("noice.lsp").scroll(2) then
            return "<c-d>"
        end
    end, { silent = true, expr = true })

    vim.keymap.set({ "n", "i", "s" }, "<c-f>", function()
        if not require("noice.lsp").scroll(-2) then
            return "<c-f>"
        end
    end, { silent = true, expr = true })
end

return M
