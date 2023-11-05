local M = {
    "saecki/crates.nvim",
    enabled = true,
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
