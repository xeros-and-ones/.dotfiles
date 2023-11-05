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
    local function get_header(headers)
        local header_text = headers[math.random(#headers)]
        return require("utils").apply_gradient_hl(header_text)
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
