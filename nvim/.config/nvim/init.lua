if vim.g.vscode then
    require "Core.options"
    require "Core.keymaps"
else
    require "Core.options"
    require "Core.Lazy"
    require "Core.keymaps"
    require "Core.autocommands"

    vim.cmd("colorscheme gruvbox")
end
