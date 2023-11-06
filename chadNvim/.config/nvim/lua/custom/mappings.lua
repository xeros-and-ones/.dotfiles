local M = {}
local fn = vim.fn
local cwd = vim.fn.stdpath "config" .. "/"
local config_dir = { cwd }
local utilities = require "custom.utilities"

M.General = {
  i = {
    ["jk"] = { "<ESC>", "escape Insert Mode", opts = { silent = true } },
    -- navigate within insert mode
    ["<C-h>"] = { "<Left>", "Move left", opts = { silent = true } },
    ["<C-l>"] = { "<Right>", "Move right", opts = { silent = true } },
    ["<C-j>"] = { "<Down>", "Move down", opts = { silent = true } },
    ["<C-k>"] = { "<Up>", "Move up", opts = { silent = true } },

    ["<C-s>"] = { "<cmd> w <CR>", "Save file", opts = { silent = true } },
  },

  n = {
    -- switch between windows
    ["<C-h>"] = { "<C-w>h", "Window left", opts = { silent = true } },
    ["<C-l>"] = { "<C-w>l", "Window right", opts = { silent = true } },
    ["<C-j>"] = { "<C-w>j", "Window down", opts = { silent = true } },
    ["<C-k>"] = { "<C-w>k", "Window up", opts = { silent = true } },

    -- save
    ["<C-s>"] = { "<cmd>w<CR>", "Save file", opts = { silent = true } },
    ["<Esc>"] = { "<cmd>noh<CR>", "Clear highlights", opts = { silent = true } },
    ["<leader>q"] = { "<cmd>q<cr>", "Quit", opts = { silent = true } },
    ["<S-q>"] = { "<cmd>Bdelete<CR>", "Close Buffer", opts = { silent = true } },
    ["<A-q>"] = {
      function()
        if vim.bo.buftype == "terminal" then
          vim.cmd "Bdelete!"
          vim.cmd "silent! close"
        elseif #vim.api.nvim_list_wins() > 1 then
          vim.cmd "silent! close"
        else
          vim.notify("Can't Close Window", vim.log.levels.WARN, { title = "Close Window" })
        end
      end,
      "Close Window",
      opts = { silent = true },
    },

    -- Resize with arrows
    ["<C-Up>"] = { "<cmd>resize +2<CR>", "resize up", opts = { silent = true } },
    ["<C-Down>"] = { "<cmd>resize -2<CR>", "resize down", opts = { silent = true } },
    ["<C-Left>"] = { "<cmd>vertical resize +2<CR>", "resize left", opts = { silent = true } },
    ["<C-Right>"] = { "<cmd>vertical resize -2<CR>", "resize right", opts = { silent = true } },

    ["p"] = { '"_dP', "Better Paste", opts = { silent = true } },
  },

  v = {
    ["p"] = { '"_dP', "Better Paste", opts = { silent = true } },
  },

  x = {
    ["<C-s>"] = { "<cmd>w<CR>", "Save file" },
    ["p"] = { 'p:let @+=@0<CR>:let @"=@0<CR>', "Dont copy replaced text", opts = { silent = true } },
  },

  c = {

    ["<tab>"] = {
      function()
        if fn.getcmdtype() == "/" or fn.getcmdtype() == "?" then
          return "<CR>/<C-r>/"
        end
        return "<C-z>"
      end,
      "Word Search Increment",
    },

    ["<S-tab>"] = {
      function()
        if fn.getcmdtype() == "/" or fn.getcmdtype() == "?" then
          return "<CR>?<C-r>/"
        end
        return "<S-Tab>"
      end,
      "Word Search Decrement",
    },
  },
}

M.Options = {
  n = {
    ["<leader>ol"] = { "<cmd>set nu!<cr>", "Toggle line number", opts = { silent = true } },
    ["<leader>or"] = { "<cmd>set rnu!<cr>", "Toggle relative number", opts = { silent = true } },
    ["<leader>ot"] = {
      function()
        require("base46").toggle_transparency()
      end,
      "Toggle Transparency",
      opts = { silent = true },
    },
    ["<leader>oL"] = { "<cmd>Lazy<cr>", "Lazy", opts = { silent = true } },
    ["<leader>om"] = { "<cmd>Mason<cr>", "Mason", opts = { silent = true } },
    ["<leader>oi"] = { "<cmd>LspInfo<cr>", "LspInfo", opts = { silent = true } },
    ["<leader>on"] = { "<cmd>NullLsInfo<cr>", "Null-LS", opts = { silent = true } },
    ["<leader>oc"] = { "<cmd>NvCheatsheet<cr>", "Cheatsheet", opts = { silent = true } },
    ["<leader>oa"] = { "<cmd>Nvdash<cr>", "Cheatsheet", opts = { silent = true } },
    ["<leader>os"] = {
      function()
        require("persistence").load()
      end,
      "Restore Last Session",
    },
  },
}

M.tabufline = {
  plugin = true,

  n = {
    -- cycle through buffers
    ["L"] = {
      function()
        require("nvchad.tabufline").tabuflineNext()
      end,
      "Goto next buffer",
      opts = { silent = true },
    },

    ["H"] = {
      function()
        require("nvchad.tabufline").tabuflinePrev()
      end,
      "Goto prev buffer",
      opts = { silent = true },
    },

    -- close buffer + hide terminal buffer
    ["<leader>x"] = {
      function()
        require("nvchad.tabufline").close_buffer()
      end,
      "Close buffer",
      opts = { silent = true },
    },
    ["<S-Left>"] = { "<cmd>tabprevious<CR>", "Go to previous tab", opts = { silent = true } },
    ["<S-Right>"] = { "<cmd>tabnext<CR>", "Go to next tab", opts = { silent = true } },
    ["<S-Up>"] = { "<cmd>tabnew<CR>", "New tab", opts = { silent = true } },
    ["<S-Down>"] = { "<cmd>tabclose<CR>", "Close tab", opts = { silent = true } },
  },
}
M.telescope = {
  plugin = true,

  n = {
    -- find
    ["<leader>ff"] = {
      function()
        require("telescope.builtin").find_files { hidden = true, no_ignore = true }
      end,
      "Find files",
      opts = { silent = true },
    },
    ["<leader>fu"] = { "<CMD>Telescope undo<CR>", "Undo History", opts = { silent = true } },
    ["<leader>fw"] = {
      function()
        require("telescope.builtin").live_grep {
          additional_args = function(args)
            return vim.list_extend(args, { "--hidden", "--no-ignore" })
          end,
        }
      end,
      "Live grep",
      opts = { silent = true },
    },
    ["<leader>fm"] = { "<cmd>Telescope marks<CR>", "Find marks", opts = { silent = true } },
    ["<leader>fb"] = { "<cmd>Telescope buffers<CR>", "Find buffers", opts = { silent = true } },
    ["<leader>fc"] = {
      function()
        require("telescope.builtin").grep_string()
      end,
      "Cursor Word",
      opts = { silent = true },
    },
    ["<leader>fh"] = { "<cmd>Telescope help_tags<CR>", "Help Tags", opts = { silent = true } },
    ["<leader>fH"] = { "<cmd>Telescope highlights<CR>", "Highlights", opts = { silent = true } },
    ["<leader>fr"] = { "<cmd>Telescope oldfiles<CR>", "Recent Files", opts = { silent = true } },
    ["<leader>fM"] = {
      function()
        require("telescope.builtin").man_pages()
      end,
      "Man Pages",
      opts = { silent = true },
    },
    ["<leader>ft"] = { "<cmd>Telescope themes<CR>", "Themes", opts = { silent = true } },
    ["<leader>fn"] = {
      function()
        require("telescope").extensions.notify.notify()
      end,
      "notifications",
      opts = { silent = true },
    },
    ["<leader>f/"] = { "<cmd>Telescope current_buffer_fuzzy_find<CR>", "Find in buffer", opts = { silent = true } },
    ["<leader>fo"] = { "<cmd>Telescope registers<CR>", "Search registers", opts = { silent = true } },
    ["<leader>fk"] = { "<cmd>Telescope keymaps<CR>", "Search keymaps", opts = { silent = true } },
    ["<leader>fC"] = { "<cmd>Telescope commands<CR>", "Search commands", opts = { silent = true } },
    ["<leader>fa"] = { "<cmd>Telescope autocommands<CR>", "Search autocommands", opts = { silent = true } },
    ["<leader>fp"] = { "<cmd>Telescope project<CR>", "Fined projects", opts = { silent = true } },
    ["<leader>fT"] = { "<cmd>TodoTrouble<CR>", "TODO", opts = { silent = true } },

    -- pick a hidden term
    ["<leader>fi"] = { "<cmd>Telescope terms<CR>", "Pick hidden term", opts = { silent = true } },
  },
}
M.Neotree = {
  plugin = true,
  n = {
    ["<leader>e"] = { "<cmd>Neotree toggle<cr>", "File Explorer", opts = { silent = true } },
  },
}

M.Neotest = {
  plugin = true,
  n = {

    ["<leader>ta"] = {
      function()
        require("neotest").run.run { suite = true, strategy = "integrated" }
        require("neotest").summary.open()
      end,
      "Run all tests",
      opts = { silent = true },
    },
    ["<leader>tt"] = {
      function()
        require("neotest").run.run { suite = false, strategy = "integrated" }
      end,
      "Run test",
      opts = { silent = true },
    },
    ["<leader>td"] = {
      function()
        require("neotest").run.run { suite = false, strategy = "dap" }
      end,
      "Debug test",
      opts = { silent = true },
    },
    ["<leader>tr"] = {
      function()
        require("neotest").run.run()
      end,
      "Run Nearest",
      opts = { silent = true },
    },
    ["<leader>ts"] = {
      function()
        require("neotest").summary.toggle()
      end,
      "Show summary",
      opts = { silent = true },
    },
    ["<leader>to"] = {
      function()
        require("neotest").output_panel.toggle()
      end,
      "Show output",
      opts = { silent = true },
    },
    ["<leader>tq"] = {
      function()
        require("neotest").run.stop()
      end,
      "Stop Tests",
      opts = { silent = true },
    },
  },
}

M.Dadbod = {
  plugin = true,
  n = {
    ["<leader>Dt"] = { "<Cmd>DBUIToggle<Cr>", "Toggle UI", opts = { silent = true } },
    ["<leader>Df"] = { "<Cmd>DBUIFindBuffer<Cr>", "Find buffer", opts = { silent = true } },
    ["<leader>Dr"] = { "<Cmd>DBUIRenameBuffer<Cr>", "Rename buffer", opts = { silent = true } },
    ["<leader>Dq"] = { "<Cmd>DBUILastQueryInfo<Cr>", "Last query info", opts = { silent = true } },
  },
}

M.Toggleterm = {
  plugin = true,
  n = {
    ["<c-\\>"] = { "<cmd>ToggleTerm direction=float<cr>", "float term", opts = { silent = true } },
    ["<c-[>"] = { "<cmd>ToggleTerm size=20 direction=horizontal<cr>", "horizontal term", opts = { silent = true } },
    ["<c-]>"] = { "<cmd>ToggleTerm size=70 direction=vertical<cr>", "vertical term", opts = { silent = true } },
  },
  t = {
    ["<c-\\>"] = { "<cmd>ToggleTerm direction=float<cr>", "float term", opts = { silent = true } },
    ["<c-[>"] = { "<cmd>ToggleTerm size=20 direction=horizontal<cr>", "horizontal term", opts = { silent = true } },
    ["<c-]>"] = { "<cmd>ToggleTerm size=70 direction=vertical<cr>", "vertical term", opts = { silent = true } },
  },
}

M.Git = {
  n = {
    ["<leader>gg"] = {
      function()
        utilities.ToggleLazygit()
      end,
      "Lazygit",
      opts = { silent = true },
    },
    ["<leader>gt"] = {
      function()
        require("telescope.builtin").git_status()
      end,
      "Git status",
      opts = { silent = true },
    },
    ["<leader>gb"] = {
      function()
        require("telescope.builtin").git_branches()
      end,
      "Git branches",
      opts = { silent = true },
    },
    ["<leader>gc"] = {
      function()
        require("telescope.builtin").git_commits()
      end,
      "Git commits",
      opts = { silent = true },
    },
    ["<leader>gd"] = {
      function()
        if next(require("diffview.lib").views) == nil then
          vim.cmd "DiffviewOpen"
        else
          vim.cmd "DiffviewClose"
        end
      end,
      "Toggle Diffview",
      opts = { silent = true },
    },
  },
}

M.ufo = {
  plugin = true,
  n = {
    ["zR"] = {
      function()
        require("ufo").openAllFolds()
      end,
      "Open all folds",
      opts = { silent = true },
    },
    ["zM"] = {
      function()
        require("ufo").closeAllFolds()
      end,
      "Close all folds",
      opts = { silent = true },
    },
    ["zr"] = {
      function()
        require("ufo").openFoldsExceptKinds()
      end,
      "Open Folds",
      opts = { silent = true },
    },
    ["zm"] = {
      function()
        require("ufo").closeFoldsWith()
      end,
      "close Folds",
      opts = { silent = true },
    },
    ["zh"] = {
      function()
        local winid = require("ufo").peekFoldedLinesUnderCursor()
        if not winid then
          return
        end
      end,
      "Fold preview",
      opts = { silent = true },
    },
  },
}

M.Tools = {
  n = {
    ["<leader>zc"] = { "<cmd>CccPick<cr>", "Colour picker", opts = { silent = true } },
    ["<leader>zs"] = {
      "<cmd>lua require('luasnip.loaders').edit_snippet_files()<cr>",
      "Edit snippets",
      opts = { silent = true },
    },
    ["<leader>zr"] = { "<cmd>luafile %<CR>", "Source current file", opts = { silent = true } },
    ["<leader>zt"] = { "<cmd>Inspect<CR>", "TS Inspect", opts = { silent = true } },
    ["<leader>zT"] = { "<cmd>InspectTree<CR>", "TS Inspect Tree", opts = { silent = true } },
    ["<leader>zO"] = { "<cmd>OverseerToggle<cr>", "Overseer List", opts = { silent = true } },
    ["<leader>zo"] = { "<cmd>OverseerRun<cr>", "Overseer Run", opts = { silent = true } },
    ["<Leader>zd"] = { "<cmd>lua require('neogen').generate()<CR>", "Generate docs", opts = { silent = true } },
    ["<Leader>zf"] = {
      function()
        require("telescope.builtin").find_files {
          prompt_title = "Config Files",
          search_dirs = config_dir,
          cwd = cwd,
        }
      end,
      "Find Config Files",
      opts = { silent = true },
    },
    ["<Leader>zg"] = {
      function()
        require("telescope.builtin").live_grep {
          prompt_title = "Config Files",
          search_dirs = config_dir,
          cwd = cwd,
        }
      end,
      "Grep Config Files",
      opts = { silent = true },
    },
    ["<Leader>zh"] = { "<cmd>messages<cr>", "Messages", opts = { silent = true } },
    ["<Leader>zm"] = { "<cmd>checkhealth<cr>", "Health", opts = { silent = true } },
    ["<Leader>za"] = { "<cmd>Nvdash<cr>", "NvDash", opts = { silent = true } },
    ["<Leader>zv"] = {
      function()
        local version = vim.version().major .. "." .. vim.version().minor .. "." .. vim.version().patch
        return vim.notify(version, vim.log.levels.INFO, { title = "Neovim Version" })
      end,
      "Version",
      opts = { silent = true },
    },
  },
}

local function show_hover()
  local filetype = vim.bo.filetype
  if vim.tbl_contains({ "vim", "help" }, filetype) then
    vim.cmd("h " .. vim.fn.expand "<cword>")
  elseif vim.tbl_contains({ "man" }, filetype) then
    vim.cmd("Man " .. vim.fn.expand "<cword>")
  elseif vim.fn.expand "%:t" == "Cargo.toml" and require("crates").popup_available() then
    require("crates").show_versions_popup()
  else
    vim.cmd "Lspsaga hover_doc"
  end
end
M.lspconfig = {
  plugin = true,
  n = {
    ["<C-`>"] = { "<cmd>TroubleToggle<cr>", "Toggle Trouble", opts = { silent = true } },
    ["<C-Space>"] = { "<cmd>NullFormat<cr>", "Format", opts = { silent = true } },
    ["<F5>"] = {
      function()
        utilities.RunCode()
      end,
      "Run Code",
      opts = { silent = true },
    },
    ["gh"] = { show_hover, "Hover Action", opts = { silent = true } },

    ["gk"] = { "<cmd>Lspsaga show_line_diagnostics<CR>", "Line Diagnostic", opts = { silent = true } },
    ["gp"] = { "<cmd>Lspsaga peek_definition<CR>", "Peek_Definition", opts = { silent = true } },
    ["gi"] = { "<cmd>Lspsaga finder imp<CR>", "LSP implementation", opts = { silent = true } },
    ["<leader>la"] = { "<cmd>Lspsaga code_action<cr>", "Code Action", opts = { silent = true } },
    ["<leader>ld"] = { "<cmd>Lspsaga goto_definition<cr>", "Goto_Definition", opts = { silent = true } },
    ["<leader>lO"] = { "<cmd>Lspsaga outline<cr>", "Code Outline", opts = { silent = true } },
    ["<leader>li"] = { "<cmd>Lspsaga incoming_calls<cr>", "Incoming Calls", opts = { silent = true } },
    ["<leader>lo"] = { "<cmd>Lspsaga outgoing_calls<cr>", "Outgoing Calls", opts = { silent = true } },
    ["]d"] = { "<cmd>Lspsaga diagnostic_jump_next<cr>", "Next Diagnostic", opts = { silent = true } },
    ["[d"] = { "<cmd>Lspsaga diagnostic_jump_prev<cr>", "Prev Diagnostic", opts = { silent = true } },
    ["<leader>lR"] = { "<cmd>LspRestart<cr>", "Restart LSP", opts = { silent = true } },
    ["<leader>lr"] = { "<cmd>Lspsaga rename<cr>", "Rename", opts = { silent = true } },
    ["<leader>lF"] = { "<cmd>Lspsaga finder tyd+ref+imp+def<cr>", "LspSaga Finder", opts = { silent = true } },
    ["<leader>lq"] = { "<cmd>TroubleToggle quickfix<cr>", "Quickfix [Trouble]", opts = { silent = true } },
    ["<leader>lD"] = { "<cmd>TroubleToggle lsp_definitions<cr>", "Definition [Trouble]", opts = { silent = true } },
    ["<leader>lf"] = { "<cmd>TroubleToggle lsp_references<cr>", "Find references [Trouble]", opts = { silent = true } },
    ["<leader>lt"] = {
      "<cmd>TroubleToggle lsp_type_definitions<cr>",
      "Type Definition [Trouble]",
      opts = { silent = true },
    },
    ["<leader>lx"] = { "<cmd>TroubleToggle document_diagnostics<cr>", "Buffer Diagnostics", opts = { silent = true } },
    ["<leader>lw"] = {
      "<cmd>TroubleToggle workspace_diagnostics<cr>",
      "Workspace Diagnostics",
      opts = { silent = true },
    },
    ["go"] = { "<cmd>Telescope lsp_document_symbols<cr>", "Buffer Symbols", opts = { silent = true } },
    ["gO"] = { "<cmd>Telescope lsp_workspace_symbols<cr>", "Workspace Symbols", opts = { silent = true } },
    ["<leader>ls"] = {
      function()
        vim.lsp.buf.signature_help()
      end,
      "LSP signature help",
      opts = { silent = true },
    },
    ["gD"] = {
      function()
        vim.lsp.buf.declaration()
      end,
      "LSP declaration",
      opts = { silent = true },
    },
    -- ["gd"] = {
    --   function()
    --     vim.lsp.buf.type_definition()
    --   end,
    --   "LSP definition type", opts = { silent = true }
    -- },

    -- ["gr"] = {
    --   function()
    --     vim.lsp.buf.references()
    --   end,
    --   "LSP references", opts = { silent = true }
    -- },

    -- ["[d"] = {
    --   function()
    --     vim.diagnostic.goto_prev { buffer = 0 }
    --   end,
    --   "Diagnostics Prev", opts = { silent = true }
    -- },

    -- ["]d"] = {
    --   function()
    --     vim.diagnostic.goto_next { buffer = 0 }
    --   end,
    --   "Diagnostics Next", opts = { silent = true }
    -- },
  },

  i = {
    ["<C-Space>"] = { "<cmd>NullFormat<cr>", "Format", opts = { silent = true } },
    ["<F5>"] = {
      function()
        utilities.RunCode()
      end,
      "Run Code",
      opts = { silent = true },
    },
  },

  v = {
    ["<C-Space>"] = { "<cmd>NullFormat<cr>", "Format", opts = { silent = true } },
    ["<F5>"] = {
      function()
        utilities.RunCode()
      end,
      "Run Code",
      opts = { silent = true },
    },
    ["<leader>la"] = { "<cmd>Lspsaga code_action<cr>", "Code Action", opts = { silent = true } },
  },
}
if vim.lsp.inlay_hint then
  M.lspconfig.n["<leader>lh"] = { "<cmd>lua vim.lsp.inlay_hint(0, nil)<cr>", "Inlay Hint", opts = { silent = true } }
end

M.comment = {
  plugin = true,

  -- toggle comment in both modes
  n = {
    ["<leader>/"] = {
      function()
        require("Comment.api").toggle.linewise.current()
      end,
      "Toggle comment",
      opts = { silent = true },
    },
  },

  v = {
    ["<leader>/"] = {
      "<ESC><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>",
      "Toggle comment",
      opts = { silent = true },
    },
  },
  x = {
    ["<leader>/"] = {
      "<ESC><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>",
      "Toggle comment",
      opts = { silent = true },
    },
  },
}

M.disabled = {}

return M
