local M = {}

-- Functional wrapper for mapping custom keybindings
M.map = function(mode, lhs, rhs, opts)
    local options = { silent = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.keymap.set(mode, lhs, rhs, options)
end
---------------------------------------------------------------------
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
---------------------------------------------------------------------
M.apply_gradient_hl = function(text)
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
---------------------------------------------------------------------
-- string padding
M.pad_string = function(str, len, align)
    local str_len = #str
    if str_len >= len then
        return str
    end

    local pad_len = len - str_len
    local pad = string.rep(" ", pad_len)

    if align == "left" then
        return str .. pad
    elseif align == "right" then
        return pad .. str
    elseif align == "center" then
        local left_pad = math.floor(pad_len / 2)
        local right_pad = pad_len - left_pad
        return string.rep(" ", left_pad) .. str .. string.rep(" ", right_pad)
    end
end
---------------------------------------------------------------------
M.ToggleLazygit = function()
    local Terminal = require("toggleterm.terminal").Terminal
    local lazygit = Terminal:new {
        cmd = "lazygit",
        hidden = true,
        on_open = function(term)
            vim.cmd "startinsert!"
        end,
        close_on_exit = true,
        direction = "float",
        float_opts = {
            border = "rounded",
        },
    }
    lazygit:toggle()
end
---------------------------------------------------------------------
local function substitute(cmd)
    cmd = cmd:gsub("%%", vim.fn.expand "%")
    cmd = cmd:gsub("$fileBase", vim.fn.expand "%:r")
    cmd = cmd:gsub("$filePath", vim.fn.expand "%:p")
    cmd = cmd:gsub("$file", vim.fn.expand "%")
    cmd = cmd:gsub("$dir", vim.fn.expand "%:p:h")
    cmd = cmd:gsub("#", vim.fn.expand "#")
    cmd = cmd:gsub("$altFile", vim.fn.expand "#")

    return cmd
end

---------------------------------------------------------------------
M.RunCode = function()
    local file_extension = vim.fn.expand "%:e"
    local selected_cmd = ""
    local term_cmd = "bot 10 new | term "
    local supported_filetypes = {
        html = {
            default = "%",
        },
        c = {
            default = "gcc % -o $fileBase && $fileBase",
            debug = "gcc -g % -o $fileBase && $fileBase",
        },
        cs = {
            default = "dotnet run",
        },
        cpp = {
            default = "g++ % -o  $fileBase && $fileBase",
            debug = "g++ -g % -o  $fileBase",
            competitive = "g++ -std=c++17 -Wall -DAL -O2 % -o $fileBase && $fileBase<input.txt",
        },
        py = {
            default = "python %",
        },
        go = {
            default = "go run %",
        },
        java = {
            default = "java %",
        },
        js = {
            default = "node %",
            debug = "node --inspect %",
        },
        ts = {
            default = "tsc % && node $fileBase",
        },
        rs = {
            default = "rustc % && $fileBase",
        },
        php = {
            default = "php %",
        },
        r = {
            default = "Rscript %",
        },
        jl = {
            default = "julia %",
        },
        rb = {
            default = "ruby %",
        },
        pl = {
            default = "perl %",
        },
    }

    if supported_filetypes[file_extension] then
        local choices = vim.tbl_keys(supported_filetypes[file_extension])

        if #choices == 0 then
            vim.notify("It doesn't contain any command", vim.log.levels.WARN, { title = "Code Runner" })
        elseif #choices == 1 then
            selected_cmd = supported_filetypes[file_extension][choices[1]]
            vim.cmd(term_cmd .. substitute(selected_cmd))
        else
            vim.ui.select(choices, { prompt = "Choose a command: " }, function(choice)
                selected_cmd = supported_filetypes[file_extension][choice]
                if selected_cmd then
                    vim.cmd(term_cmd .. substitute(selected_cmd))
                end
            end)
        end
    else
        vim.notify("The filetype isn't included in the list", vim.log.levels.WARN, { title = "Code Runner" })
    end
end

return M
