local M = {
    "hrsh7th/nvim-cmp",
    enabled = true,
    dependencies = {
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-buffer",
        "FelipeLema/cmp-async-path",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-emoji",
        "saadparwaiz1/cmp_luasnip",
        "hrsh7th/cmp-cmdline",
        {
            "rafamadriz/friendly-snippets",
            config = function()
                require("luasnip.loaders.from_vscode").lazy_load {
                    require("luasnip").filetype_extend("python", { "django", "pydoc" }),
                    require("luasnip").filetype_extend("htmldjango", { "html", "djangohtml" }),

                    -- vscode format
                    require("luasnip.loaders.from_vscode").lazy_load(),
                    require("luasnip.loaders.from_vscode").lazy_load { paths = vim.g.vscode_snippets_path or "" },

                    -- snipmate format
                    require("luasnip.loaders.from_snipmate").load(),
                    require("luasnip.loaders.from_snipmate").lazy_load { paths = vim.g.snipmate_snippets_path or "" },

                    -- lua format
                    require("luasnip.loaders.from_lua").load(),
                    require("luasnip.loaders.from_lua").lazy_load { paths = vim.g.lua_snippets_path or "" },
                }
            end,
        },
        {
            "L3MON4D3/LuaSnip",
            -- follow latest release.
            version = "v2.*",
            build = "make install_jsregexp",
            opts = {
                history = true,
                update_events = "TextChanged,TextChangedI",
                delete_check_events = "TextChanged,InsertLeave",
                enable_autosnippets = true,
            },
        },
    },

    event = { "InsertEnter", "CmdlineEnter" },
}

local has_words_before = function()
    if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
        return false
    end
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match "^%s*$" == nil
end

-- Kind icons instead of using lspkind
local kind_icons = {
    Text = "󰉿",
    Method = "󰆧",
    Function = "󰊕",
    Constructor = "",
    Field = "󰜢",
    Variable = "󰀫",
    Class = "󰠱",
    Interface = "",
    Module = "",
    Property = "󰜢",
    Unit = "󰑭",
    Value = "󰎠",
    Enum = "",
    Keyword = "󰌋",
    Snippet = "",
    Color = "󰏘",
    File = "󰈙",
    Reference = "󰈇",
    Folder = "󰉋",
    EnumMember = "",
    Constant = "󰏿",
    Struct = "󰙅",
    Event = "",
    Operator = "󰆕",
    TypeParameter = "",
}

function M.config()
    local cmp = require "cmp"
    local luasnip = require "luasnip"

    cmp.setup {
        completion = {
            completeopt = "menu,menuone,noinsert ",
        },
        snippet = {
            expand = function(args)
                luasnip.lsp_expand(args.body)
            end,
        },
        experimental = {
            ghost_text = true,
            native_menu = false,
        },
        confirm_opts = {
            behavior = cmp.ConfirmBehavior.Replace,
            select = false,
        },
        window = {
            completion = cmp.config.window.bordered {
                winhighlight = "Normal:Float",
                border = "rounded",
            },
            documentation = cmp.config.window.bordered {
                winhighlight = "Normal:Float",
                border = "rounded",
            },
        },
        mapping = {
            ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select }, { "i", "c" }),
            ["<Down>"] = cmp.mapping(
                cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
                { "i", "c" }
            ),
            ["<C-k>"] = cmp.mapping(
                cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
                { "i", "c" }
            ),
            ["<C-j>"] = cmp.mapping(
                cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
                { "i", "c" }
            ),
            ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
            ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
            ["<c-space>"] = cmp.mapping {
                i = cmp.mapping.complete(),
                c = function(_) -- fallback
                    if cmp.visible() then
                        if not cmp.confirm { select = true } then
                            return
                        end
                    else
                        cmp.complete()
                    end
                end,
            },
            ["<C-y>"] = cmp.mapping(
                cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Insert, select = true },
                { "i", "c" }
            ),
            ["<C-e>"] = cmp.mapping.close(),
            ["<CR>"] = cmp.mapping.confirm {
                behavior = cmp.ConfirmBehavior.Replace,
                select = true,
            },
            ["<Tab>"] = cmp.mapping(function(fallback)
                -- if cmp.visible() then
                --     cmp.select_next_item()
                if luasnip.expandable() then
                    luasnip.expand()
                elseif luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                elseif require("neogen").jumpable(1) then
                    require("neogen").jump_next()
                elseif has_words_before() then
                    cmp.complete()
                else
                    fallback()
                end
            end, {
                "i",
                "s",
            }),
            ["<S-Tab>"] = cmp.mapping(function(fallback)
                -- if cmp.visible() then
                --     cmp.select_prev_item()
                if luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                elseif require("neogen").jumpable(-1) then
                    require("neogen").jump_prev()
                else
                    fallback()
                end
            end, {
                "i",
                "s",
            }),
        },
        sources = {
            { name = "nvim_lsp" },
            { name = "luasnip", option = { show_autosnippets = true } },
            { name = "buffer" },
            { name = "async_path" },
            { name = "crates" },
            { name = "nvim_lua", ft = "lua" },
            { name = "emoji" },
        },
        formatting = {
            fields = { --[[ "menu", ]]
                "abbr",
                "kind",
            },
            format = function(entry, vim_item)
                --     -- concatonates the icons with the name of the item kind
                vim_item.kind = string.format("%s %s", kind_icons[vim_item.kind], vim_item.kind)
                -- vim_item.kind = string.format("%s", kind_icons[vim_item.kind])
                --     vim_item.menu = ("")
                --     -- ({
                --     --     nvim_lsp = '[LSP]',
                --     --     luasnip = '[SNIP]',
                --     --     buffer = '[BUFF]',
                --     --     path = '[PATH]',
                --     --     nvim_lua = '[LUA]',
                --     -- })[entry.source.name]
                --     -- vim_item.dup = 0
                return vim_item
            end,
        },
        sorting = {
            comparators = {
                cmp.config.compare.offset,
                cmp.config.compare.exact,
                cmp.config.compare.score,

                -- copied from cmp-under, but I don't think I need the plugin for this.
                -- I might add some more of my own.
                function(entry1, entry2)
                    local _, entry1_under = entry1.completion_item.label:find "^_+"
                    local _, entry2_under = entry2.completion_item.label:find "^_+"
                    entry1_under = entry1_under or 0
                    entry2_under = entry2_under or 0
                    if entry1_under > entry2_under then
                        return false
                    elseif entry1_under < entry2_under then
                        return true
                    end
                end,

                cmp.config.compare.kind,
                cmp.config.compare.sort_text,
                cmp.config.compare.length,
                cmp.config.compare.order,
            },
        },
    }

    cmp.setup.cmdline({ "/", "?" }, {
        sources = {
            { name = "buffer", keyword_length = 1 },
        },
    })
    cmp.setup.cmdline(":", {
        sources = {
            { name = "path" },
            { name = "nvim_lua" },
            { name = "cmdline", keyword_length = 2 },
        },
    })

    local cmp_autopairs = require "nvim-autopairs.completion.cmp"
    cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done {})
end

return M
