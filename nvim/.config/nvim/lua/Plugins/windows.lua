local M = {
    "anuvyklack/windows.nvim",
    dependencies = { "anuvyklack/middleclass" },
    config = function()
        require('windows').setup()
    end
}

return M
