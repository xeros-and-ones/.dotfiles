-- Copyright (c) 2021 Ashish Panigrahi
-- MIT license, see LICENSE for more details.
-- stylua: ignore
local colors = {
    black        = '#202020',
    yellow       = '#d79921',
    white        = '#FFFFFF',
    green        = '#b8bb26',
    purple       = '#b16286',
    blue         = '#83a598',
    darkblue     = '#458588',
    navyblue     = '#326173',
    brightgreen  = '#8ec07c',
    gray         = '#444444',
    darkgray     = '#3c3836',
    lightgray    = '#bab4ae',
    inactivegray = '#7c6f64',
    orange       = '#d65d0e',
    red          = '#cc241d',
    brightorange = '#C08A20',
    brightred    = '#fb4934',
    cyan         = '#689d6a',
}

return {
    normal = {
        a = { bg = colors.yellow, fg = colors.black, gui = "bold" },
        b = { bg = colors.gray, fg = colors.lightgray },
        c = { bg = "NONE", fg = colors.white },
    },
    insert = {
        a = { bg = colors.blue, fg = colors.black, gui = "bold" },
        b = { bg = colors.darkgray, fg = colors.lightgray },
        c = { bg = "NONE", fg = colors.white },
    },
    visual = {
        a = { bg = colors.orange, fg = colors.black, gui = "bold" },
        b = { bg = colors.gray, fg = colors.lightgray },
        c = { bg = "NONE", fg = colors.white },
    },
    replace = {
        a = { bg = colors.brightred, fg = colors.black, gui = "bold" },
        b = { bg = colors.gray, fg = colors.lightgray },
        c = { bg = "NONE", fg = colors.white },
    },
    command = {
        a = { bg = colors.green, fg = colors.black, gui = "bold" },
        b = { bg = colors.gray, fg = colors.lightgray },
        c = { bg = "NONE", fg = colors.brightgreen },
    },
    inactive = {
        a = { bg = colors.purple, fg = colors.lightgray, gui = "bold" },
        b = { bg = colors.darkgray, fg = colors.lightgray },
        c = { bg = "NONE", fg = colors.white },
    },
}
