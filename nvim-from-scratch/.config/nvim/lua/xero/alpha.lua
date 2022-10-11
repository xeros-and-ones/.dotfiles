local status_ok, alpha = pcall(require, "alpha")
if not status_ok then
  return
end

local dashboard = require "alpha.themes.dashboard"
dashboard.section.header.val = {
  [[                                     ]],
  [[                                     ]],
  [[░██╗░░██╗░███████╗░██████╗░░░█████╗░░]],
  [[░╚██╗██╔╝░██╔════╝░██╔══██╗░██╔══██╗░]],
  [[░░╚███╔╝░░█████╗░░░██████╔╝░██║░░██║░]],
  [[░░██╔██╗░░██╔══╝░░░██╔══██╗░██║░░██║░]],
  [[░██╔╝╚██╗░███████╗░██║░░██║░╚█████╔╝░]],
  [[░╚═╝░░╚═╝░╚══════╝░╚═╝░░╚═╝░░╚════╝░░]],
  [[                                     ]],
}
dashboard.section.buttons.val = {
  dashboard.button("f", " " .. " Find file", "<cmd>lua require('telescope.builtin').find_files({hidden = true, no_ignore = true})<CR>"),
  dashboard.button("e", " " .. " New file", ":ene <BAR> startinsert <CR>"),
  dashboard.button("p", " " .. " Find project", ":lua require('telescope').extensions.projects.projects()<CR>"),
  dashboard.button("r", " " .. " Recent files", ":Telescope oldfiles <CR>"),
  dashboard.button("t", " " .. " Find text", "<cmd>lua require('telescope.builtin').live_grep({ additional_args = function(args) return vim.list_extend(args, { '--hidden', '--no-ignore' })end,})<cr>"),
  dashboard.button("c", " " .. " Config", ":e ~/.config/nvim/init.lua <CR>"),
  dashboard.button("q", " " .. " Quit", ":qa<CR>"),
}
local function footer()
  return "mohamed96tarek@hotmail.com"
end

dashboard.section.footer.val = footer()

dashboard.section.footer.opts.hl = "Type"
dashboard.section.header.opts.hl = "Include"
dashboard.section.buttons.opts.hl = "Keyword"

dashboard.opts.opts.noautocmd = true
alpha.setup(dashboard.opts)
