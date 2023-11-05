local M = {
    "kevinhwang91/nvim-ufo",
    event = "VeryLazy",
    enabled = true,
    dependencies = {
        "kevinhwang91/promise-async",
        {
            "luukvbaal/statuscol.nvim",
            event = "BufReadPost",
            config = function()
                local builtin = require "statuscol.builtin"
                require("statuscol").setup {
                    relculright = true,
                    bt_ignore = {
                        "nofile",
                        "prompt",
                        "terminal",
                        "packer",
                        "alpha",
                        "Alpha",
                        "dashboard",
                        "Dashboard",
                        "Scratch",
                        "scratch",
                    },
                    ft_ignore = {
                        "Scratch",
                        "scratch",
                        "alpha",
                        "Alpha",
                        "dashboard",
                        "Dashboard",
                        "dapui_watches",
                        "dap-repl",
                        "dapui_console",
                        "dapui_stacks",
                        "dapui_breakpoints",
                        "dapui_scopes",
                        "help",
                        "vim",
                        "neo-tree",
                        "noice",
                        "toggleterm",
                    },
                    segments = {
                        { text = { "%s" }, click = "v:lua.ScSa" },
                        { text = { builtin.foldfunc, " " }, click = "v:lua.ScFa" },
                        { text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
                    },
                }
            end,
        },
    },
}

M.config = function()
    local handler = function(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local suffix = (" ⋯⋯    %d  ⋯⋯  "):format(endLnum - lnum)
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        for _, chunk in ipairs(virtText) do
            local chunkText = chunk[1]
            local chunkWidth = vim.fn.strdisplaywidth(chunkText)
            if targetWidth > curWidth + chunkWidth then
                table.insert(newVirtText, chunk)
            else
                chunkText = truncate(chunkText, targetWidth - curWidth)
                local hlGroup = chunk[2]
                table.insert(newVirtText, { chunkText, hlGroup })
                chunkWidth = vim.fn.strdisplaywidth(chunkText)

                if curWidth + chunkWidth < targetWidth then
                    suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
                end
                break
            end
            curWidth = curWidth + chunkWidth
        end
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
    end

    local ftMap = {
        vim = "indent",
        python = { "indent" },
        git = "",
    }

    require("ufo").setup {
        fold_virt_text_handler = handler,
        open_fold_hl_timeout = 150,
        close_fold_kinds = { "imports", "comment" },

        preview = {
            win_config = {
                border = "rounded",
                winhighlight = "Normal:Folded",
                winblend = 0,
            },
            mappings = {
                scrollU = "<C-f>",
                scrollD = "<C-d>",
                close = { "<esc>", "q" },
            },
        },

        provider_selector = function(bufnr, filetype, buftype)
            return ftMap[filetype] or { "treesitter", "indent" }
        end,
    }
end

return M
