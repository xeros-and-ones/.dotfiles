local M = {
    "goolord/alpha-nvim",
    commit = "712dc1dccd4fd515ef8bd126b3718f79d3e23b0d",
    enabled = true,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VimEnter",
}

function M.config()
    local headers = require "headers"
    local theme = require "alpha.themes.theta"
    local path_ok, _ = pcall(require, "plenary.path")
    if not path_ok then
        return
    end

    math.randomseed(os.time())

    -- Header
    -- string padding
    -- local pad_string = function(str, len, align)
    --     local str_len = #str
    --     if str_len >= len then
    --         return str
    --     end

    --     local pad_len = len - str_len
    --     local pad = string.rep(" ", pad_len)

    --     if align == "left" then
    --         return str .. pad
    --     elseif align == "right" then
    --         return pad .. str
    --     elseif align == "center" then
    --         local left_pad = math.floor(pad_len / 2)
    --         local right_pad = pad_len - left_pad
    --         return string.rep(" ", left_pad) .. str .. string.rep(" ", right_pad)
    --     end
    -- end
    local create_gradient = function(start, finish, steps)
        local r1, g1, b1 =
            tonumber("0x" .. start:sub(2, 3)), tonumber("0x" .. start:sub(4, 5)), tonumber("0x" .. start:sub(6, 7))
        local r2, g2, b2 =
            tonumber("0x" .. finish:sub(2, 3)), tonumber("0x" .. finish:sub(4, 5)), tonumber("0x" .. finish:sub(6, 7))

        local r_step = (r2 - r1) / steps
        local g_step = (g2 - g1) / steps
        local b_step = (b2 - b1) / steps

        local gradient = {}
        for i = 1, steps do
            local r = math.floor(r1 + r_step * i)
            local g = math.floor(g1 + g_step * i)
            local b = math.floor(b1 + b_step * i)
            table.insert(gradient, string.format("#%02x%02x%02x", r, g, b))
        end

        return gradient
    end
    local apply_gradient_hl = function(text)
        local gradient = create_gradient("#DCA561", "#658594", #text)

        local lines = {}
        for i, line in ipairs(text) do
            local tbl = {
                type = "text",
                val = line,
                opts = {
                    hl = "HeaderGradient" .. i,
                    shrink_margin = false,
                    position = "center",
                },
            }
            table.insert(lines, tbl)

            -- create hl group
            vim.api.nvim_set_hl(0, "HeaderGradient" .. i, { fg = gradient[i] })
        end

        return {
            type = "group",
            val = lines,
            opts = { position = "center" },
        }
    end
    local function get_header(headers)
        local header_text = headers[math.random(#headers)]
        return apply_gradient_hl(header_text)
    end

    -- Info section
    local function get_info()
        local lazy_stats = require("lazy").stats()
        local total_plugins = " " .. lazy_stats.loaded .. "/" .. lazy_stats.count .. " packages"
        local datetime = os.date " %A %B %d"
        local version = vim.version()
        local nvim_version_info = "ⓥ " .. version.major .. "." .. version.minor .. "." .. version.patch

        local info_string = datetime .. "  |  " .. total_plugins .. "  |  " .. nvim_version_info

        return {
            type = "text",
            val = info_string,
            opts = {
                hl = "Delimiter",
                position = "center",
            },
        }
    end

    -- Links / tools
    local dashboard = require "alpha.themes.dashboard"
    local links = {
        type = "group",
        val = {

            dashboard.button("f", " " .. " Find file", ":Telescope find_files <CR>"),
            dashboard.button("r", "󰄉 " .. " Recent files", ":Telescope oldfiles <CR>"),
            dashboard.button("n", " " .. " New File", ":enew <CR>"),
            dashboard.button(
                "s",
                " " .. " Restore Session",
                "<CMD>lua require('persistence').load({ last = true })<CR>"
            ),
            dashboard.button("t", " " .. " Find text", ":Telescope live_grep <CR>"),
            dashboard.button("l", "󰒲 " .. " Lazy", "<cmd>Lazy<CR>"),
            dashboard.button("m", " " .. " Mason", "<cmd>Mason<CR>"),
            dashboard.button("c", " " .. " Config", ":e $MYVIMRC <CR>"),
            dashboard.button("q", " " .. " Quit", ":qa<CR>"),
        },
        position = "center",
        --opts = { spacing = 1 },
    }

    -- MRU
    local function get_mru(max_shown)
        local tbl = {
            { type = "text", val = "Recent Files", opts = { hl = "SpecialComment", position = "center" } },
        }

        local mru_list = theme.mru(1, "", max_shown)
        for _, file in ipairs(mru_list.val) do
            table.insert(tbl, file)
        end

        return { type = "group", val = tbl, opts = { width = 120 } }
    end

    -- Layout
    theme.config.layout = {
        { type = "padding", val = 4 },
        { type = "padding", val = 4 },
        get_header {
            headers.header,
            headers.cool,
            headers.panda,
            headers.xero1,
            headers.xero2,
            headers.xerobig,
            headers.beautiful,
        },
        { type = "padding", val = 1 },
        links,
        { type = "padding", val = 2 },
        get_mru(7),
        { type = "padding", val = 3 },
    }
    require("alpha").setup(theme.config)
end

return M
