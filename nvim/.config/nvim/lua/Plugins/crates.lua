local M = {
    "saecki/crates.nvim",
    commit = "7e0e24b5c28c9fababf2b965f5840e6867c96848",
    enabled = true,
    dependencies = "nvim-lua/plenary.nvim",
    event = "BufRead Cargo.toml",
}

function M.config()
    require("crates").setup {
        null_ls = {
            enabled = true,
            name = "Crates",
        },
        src = {
            cmp = {
                enabled = true,
            },
        },
        popup = {
            autofocus = true,
            hide_on_select = true,
        },
    }
    require("crates").show()
end

return M
