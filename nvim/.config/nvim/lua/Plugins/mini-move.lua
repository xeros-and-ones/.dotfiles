local M = {
    'echasnovski/mini.move',
    version = false,
    event = "BufRead"
}

M.opts = {
    -- Module mappings. Use `''` (empty string) to disable one.
    mappings = {
        -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
        left = '<A-Left>',
        right = '<A-Right>',
        down = '<A-Down>',
        up = '<A-Up>',

        -- Move current line in Normal mode
        line_left = '<A-Left>',
        line_right = '<A-Right>',
        line_down = '<A-Down>',
        line_up = '<A-Up>',
    },

    -- Options which control moving behavior
    options = {
        -- Automatically reindent selection during linewise vertical move
        reindent_linewise = true,
    },
}


return M
