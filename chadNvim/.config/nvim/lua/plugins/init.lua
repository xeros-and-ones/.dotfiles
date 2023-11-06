-- All plugins have lazy=true by default,to load a plugin on startup just lazy=false
-- List of all default plugins & their definitions
local default_plugins = {

  "nvim-lua/plenary.nvim",

  {
    "NvChad/base46",
    branch = "v2.0",
    build = function()
      require("base46").load_all_highlights()
    end,
  },

  {
    "NvChad/ui",
    branch = "v2.0",
    lazy = false,
  },

  -------------------------------------------------------------------------------
  {
    "gbprod/stay-in-place.nvim",
    enabled = true,
    event = { "BufReadPost", "BufNewFile" },
    opts = { set_keymaps = true, preserve_visual_selection = true },
  },
  -------------------------------------------------------------------------------
  {
    "folke/persistence.nvim",
    event = "BufReadPre",
    opts = {
      dir = vim.fn.expand(vim.fn.stdpath "state" .. "/sessions/"), -- directory where session files are saved
      options = { "buffers", "curdir", "folds", "tabpages", "winsize", "globals" }, -- sessionoptions used for saving
      pre_save = nil, -- a function to call before saving the session
    },
  },

  -----------------------------------------------------------------
  {
    "anuvyklack/hydra.nvim",
    enabled = true,
    event = "VimEnter",
  },
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  {
    "RRethy/vim-illuminate",
    event = { "CursorHold", "CursorHoldI" },
    config = function()
      require("illuminate").configure {
        under_cursor = true,
        max_file_lines = nil,
        delay = 100,
        providers = {
          "lsp",
          "treesitter",
          "regex",
        },
        filetypes_denylist = {
          "NvimTree",
          "Trouble",
          "Outline",
          "TelescopePrompt",
          "Empty",
          "dirvish",
          "fugitive",
          "alpha",
          "packer",
          "neogitstatus",
          "spectre_panel",
          "toggleterm",
          "DressingSelect",
          "aerial",
        },
      }
    end,
  },

  -- UI for messages, cmdline, and popup
  {
    "folke/noice.nvim",
    opts = require "plugins.configs.noice",
  },

  {
    "echasnovski/mini.move",
    version = false,
    event = "BufRead",
    opts = {
      -- Module mappings. Use `''` (empty string) to disable one.
      mappings = {
        -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
        left = "<A-h>",
        right = "<A-l>",
        down = "<A-j>",
        up = "<A-k>",

        -- Move current line in Normal mode
        line_left = "<A-h>",
        line_right = "<A-l>",
        line_down = "<A-j>",
        line_up = "<A-k>",
      },

      -- Options which control moving behavior
      options = {
        -- Automatically reindent selection during linewise vertical move
        reindent_linewise = true,
      },
    },
  },

  {
    "echasnovski/mini.splitjoin",
    version = false,
    enabled = true,
    opts = {
      mappings = {
        toggle = "gs",
        split = "",
        join = "",
      },
    },
  },

  {
    "danymat/neogen",
    enabled = true,
    event = { "BufRead", "BufNewFile" },
    opts = {
      snippet_engine = "luasnip",
      languages = {
        python = {
          template = {
            annotation_convention = "google_docstrings",
          },
        },
      },
    },
  },

  -------------------------------------------------------------------------------
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = require "plugins.configs.flash",
  },
  -------------------------------------------------------------------------------
  -- Buffer Delete
  {
    "moll/vim-bbye",
    cmd = { "Bdelete", "Bwipeout" },
  },

  -----------------------------------------------------------------
  {
    "folke/todo-comments.nvim",
    enabled = true,
    event = "VeryLazy",
    opts = require "plugins.configs.todo-comments",
    init = function()
      require("core.utils").load_mappings "comment"
    end,
  },
  {
    "nvim-neotest/neotest",
    enabled = true,
    dependencies = {
      "nvim-neotest/neotest-python",
      "nvim-neotest/neotest-go",
      "rouge8/neotest-rust",
      "Issafalcon/neotest-dotnet",
    },
    event = { "BufReadPost", "BufNewFile" },
    init = function()
      require("core.utils").load_mappings "Neotest"
    end,
    config = function()
      require "plugins.configs.neotest"
    end,
  },
  -----------------------------------------------------------------
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    enabled = true,
    init = function()
      require("core.utils").lazy_load "indent-blankline.nvim"
      require("core.utils").load_mappings "blankline"
    end,
    config = function()
      dofile(vim.g.base46_cache .. "blankline")
      require("ibl").setup {
        exclude = {
          filetypes = {
            "help",
            "terminal",
            "lazy",
            "lspinfo",
            "TelescopePrompt",
            "TelescopeResults",
            "mason",
            "nvdash",
            "nvcheatsheet",
            "",
          },
          buftypes = { "terminal" },
        },
        indent = { char = "▏", smart_indent_cap = true },
        scope = {
          enabled = true,
          show_start = true,
          show_end = false,
          injected_languages = true,
        },
      }
    end,
  },
  -----------------------------------------------------------------
  -- Schemas
  { "b0o/schemastore.nvim" },
  -----------------------------------------------------------------
  {
    "stevearc/overseer.nvim",
    cmd = { "OverseerRun", "OverseerToggle" },
    config = function()
      require("overseer").setup {
        strategy = {
          "toggleterm",
          -- load your default shell before starting the task
          use_shell = true,
          -- overwrite the default toggleterm "direction" parameter
          direction = "horizontal",
          close_on_exit = false,
          -- open the toggleterm window when a task starts
          open_on_start = true,
          -- mirrors the toggleterm "hidden" parameter, and keeps the task from
          -- being rendered in the toggleable window
          hidden = false,
        },
        dap = true,
        auto_scroll = true,
        close_on_exit = false,
        open_on_start = true,
        templates = { "builtin", "scripts", "python" },
      }
    end,
  },
  -----------------------------------------------------------------
  {
    "tpope/vim-dadbod",
    enabled = true,
    dependencies = {
      "kristijanhusak/vim-dadbod-ui",
      "kristijanhusak/vim-dadbod-completion",
    },
    cmd = { "DBUIToggle", "DBUI", "DBUIAddConnection", "DBUIFindBuffer", "DBUIRenameBuffer", "DBUILastQueryInfo" },

    init = function()
      require("core.utils").lazy_load "vim-dadbod"
      require("core.utils").load_mappings "Dadbod"
    end,
    config = function()
      local function db_completion()
        require("cmp").setup.buffer { sources = { { name = "vim-dadbod-completion" } } }
      end
      vim.g.db_ui_save_location = vim.fn.stdpath "config" .. require("plenary.path").path.sep .. "db_ui"

      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "sql",
        },
        command = [[setlocal omnifunc=vim_dadbod_completion#omni]],
      })

      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "sql",
          "mysql",
          "plsql",
        },
        callback = function()
          vim.schedule(db_completion)
        end,
      })
    end,
  },
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  {
    "hiphish/rainbow-delimiters.nvim",
    enabled = false,
    event = "BufReadPost",
    config = function()
      local rainbow_delimiters = require "rainbow-delimiters"

      vim.g.rainbow_delimiters = {
        strategy = {
          [""] = rainbow_delimiters.strategy["global"],
          vim = rainbow_delimiters.strategy["local"],
        },
        query = {
          [""] = "rainbow-delimiters",
          lua = "rainbow-blocks",
        },
        highlight = {
          "RainbowDelimiterRed",
          "RainbowDelimiterYellow",
          "RainbowDelimiterBlue",
          "RainbowDelimiterOrange",
          "RainbowDelimiterGreen",
          "RainbowDelimiterViolet",
          "RainbowDelimiterCyan",
        },
      }
    end,
  },
  -------------------------------------------------------------------------------
  {
    "uga-rosa/ccc.nvim",
    cmd = "CccPick",

    init = function()
      require("core.utils").lazy_load "ccc.nvim"
    end,
    config = function()
      local mapping = require("ccc").mapping
      require("ccc").setup {
        default_color = "#40bfbf",
        highlighter = { auto_enable = true },
        save_on_quit = true,
        inputs = { require("ccc").input.hsl, require("ccc").input.rgb },
        recognize = { input = false, output = true },
        win_opts = {
          relative = "cursor",
          style = "minimal",
          border = "solid",
        },
        mappings = {
          ["p"] = mapping.toggle_prev_colors,
        },
      }
    end,
  },
  -------------------------------------------------------------------------------
  -- Tmux navigation
  {
    "alexghergh/nvim-tmux-navigation",
    enabled = false,
    event = "VeryLazy",
    opts = {
      keybindings = {
        left = "<C-h>",
        down = "<C-j>",
        up = "<C-k>",
        right = "<C-l>",
        last_active = "<C-\\>",
        next = "<C-Space>",
      },
    },
  },
  -------------------------------------------------------------------------------

  {
    "kevinhwang91/nvim-ufo",
    event = "VeryLazy",
    enabled = true,
    dependencies = {
      "kevinhwang91/promise-async",
      {
        "luukvbaal/statuscol.nvim",
        event = "BufReadPost",
        config = function()
          local builtin = require "statuscol.builtin"
          require("statuscol").setup {
            relculright = true,
            bt_ignore = {
              "nofile",
              "prompt",
              "terminal",
              "packer",
            },
            ft_ignore = {
              "dapui_watches",
              "dap-repl",
              "dapui_console",
              "dapui_stacks",
              "dapui_breakpoints",
              "dapui_scopes",
              "help",
              "vim",
              "neo-tree",
              "noice",
              "toggleterm",
            },
            segments = {
              { text = { "%s" }, click = "v:lua.ScSa" },
              { text = { builtin.foldfunc, " " }, click = "v:lua.ScFa" },
              { text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
            },
          }
        end,
      },
    },
    init = function()
      require("core.utils").load_mappings "ufo"
    end,
    config = function()
      require "plugins.configs.folds"
    end,
  },
  --------------------------------------------------------------------------------------------
  {
    "akinsho/toggleterm.nvim",
    cmd = {
      "ToggleTerm",
      "ToggleTermSendCurrentLine",
      "ToggleTermSendVisualLines",
      "ToggleTermSendVisualSelection",
    },
    init = function()
      require("core.utils").load_mappings "Togglterm"
    end,
    config = function()
      require "plugins.configs.toggleterm"
    end,
  },
  --------------------------------------------------------------------------------------------
  {
    "nvim-tree/nvim-web-devicons",
    opts = function()
      return { override = require "nvchad.icons.devicons" }
    end,
    config = function(_, opts)
      dofile(vim.g.base46_cache .. "devicons")
      require("nvim-web-devicons").setup(opts)
    end,
  },
  --------------------------------------------------------------------------------------------

  --------------------------------------------------------------------------------------------
  {
    "nvim-treesitter/nvim-treesitter",
    init = function()
      require("core.utils").lazy_load "nvim-treesitter"
    end,
    cmd = {
      "TSInstall",
      "TSUninstall",
      "TSInstallInfo",
      "TSUpdate",
      "TSBufEnable",
      "TSBufDisable",
      "TSEnable",
      "TSDisable",
      "TSModuleInfo",
      "TSToggle",
      "TSBufToggle",
    },
    build = function()
      require("nvim-treesitter.install").update { with_sync = true }
    end,
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },

    config = function()
      require "plugins.configs.treesitter"
    end,
  },
  -------------------------------------------------------------------------------

  -------------------------------------------------------------------------------

  -------------------------------------------------------------------------------

  -------------------------------------------------------------------------------
  {
    "nvim-pack/nvim-spectre",
    cmd = "Spectre",

    opts = { open_cmd = "noswapfile vnew" },
    keys = {
      {
        "<leader>st",
        function()
          require("spectre").toggle()
        end,
        desc = "Replace in files (Spectre)",
      },
      {
        "<leader>sc",
        '<cmd>lua require("spectre").open_file_search({select_word=true})<CR>',
        desc = "Search on current file",
      },
    },
  },
  -----------------------------------------------------------------
  -- git stuff
  {

    "lewis6991/gitsigns.nvim",
    enabled = true,
    event = { "BufReadPost", "BufNewFile" },
    dependencies = {
      {
        "sindrets/diffview.nvim",
        event = "VeryLazy",
        config = true,
      },
    },
    init = function()
      -- load gitsigns only when a git file is opened
      vim.api.nvim_create_autocmd({ "BufRead" }, {
        group = vim.api.nvim_create_augroup("GitSignsLazyLoad", { clear = true }),
        callback = function()
          vim.fn.system("git -C " .. '"' .. vim.fn.expand "%:p:h" .. '"' .. " rev-parse")
          if vim.v.shell_error == 0 then
            vim.api.nvim_del_augroup_by_name "GitSignsLazyLoad"
            vim.schedule(function()
              require("lazy").load { plugins = { "gitsigns.nvim" } }
            end)
          end
        end,
      })
    end,

    config = function()
      require "plugins.configs.gitsigns"
    end,
  },

  -----------------------------------------------------------------
  {
    "neovim/nvim-lspconfig",
    init = function()
      require("core.utils").lazy_load "nvim-lspconfig"
    end,
    config = function()
      require "plugins.configs.lspconfig"
    end,

    dependencies = {
      {
        "williamboman/mason.nvim",
        cmd = {
          "Mason",
          "MasonInstall",
          "MasonUpdate",
          "MasonUninstall",
          "MasonUninstallAll",
          "MasonLog",
        },
        opts = function()
          return require "plugins.configs.mason"
        end,
        config = function(_, opts)
          dofile(vim.g.base46_cache .. "mason")
          require("mason").setup(opts)

          -- custom nvchad cmd to install all mason binaries listed
          vim.api.nvim_create_user_command("MasonInstallAll", function()
            vim.cmd("MasonInstall " .. table.concat(opts.ensure_installed, " "))
          end, {})

          vim.g.mason_binaries_list = opts.ensure_installed
        end,
      },
      {
        "williamboman/mason-lspconfig.nvim",
      },
      {
        "nvimdev/lspsaga.nvim",
        init = function()
          require("core.utils").load_mappings "Lspsaga"
        end,
        opts = require "plugins.configs.lspsaga",
      },
      -- For Plugin Development
      {
        "folke/neodev.nvim",
        opts = require "plugins.configs.neodev",
      },
      -- Start/Stop LSP when focus is lost/gained
      -- {
      --   "hinell/lsp-timeout.nvim",
      --   config = function()
      --     vim.g["lsp-timeout-config"] = {
      --       stopTimeout = 0,
      --       startTimeout = 1,
      --       silent = true, -- true to suppress notifications
      --     }
      --   end,
      -- },
      {
        "nvimtools/none-ls.nvim",
        event = "BufReadPre",
        dependecies = {
          {
            "ThePrimeagen/refactoring.nvim",
            config = function()
              require("refactoring").setup()
            end,
          },
        },
        config = function()
          require "plugins.configs.none_ls"
        end,
      },
      {
        "mrcjkb/rustaceanvim",
        version = "^3", -- Recommended
        init = function()
          -- Configure rustaceanvim here
          vim.g.rustaceanvim = {}
        end,
        ft = { "rust" },
      },
    },
  },

  -------------------------------------------------------------------------------
  {
    "stevearc/dressing.nvim",
    enabled = true,
    event = "VeryLazy",
    opts = {
      input = {
        enabled = true,
        default_prompt = "➤ ",
        win_options = {
          winblend = 0,
        },
      },
      select = {
        enabled = true,
        backend = { "telescope", "builtin" },
        builtin = {
          win_options = {
            winblend = 0,
          },
        },
      },
    },
  },
  -----------------------------------------------------------------
  -- load luasnips + cmp related in insert mode only
  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      {
        -- snippet plugin
        "L3MON4D3/LuaSnip",
        dependencies = "rafamadriz/friendly-snippets",
        config = function()
          require("luasnip").config.set_config {
            history = true,
            update_events = "TextChanged,TextChangedI",
            delete_check_events = "TextChanged,InsertLeave",
            enable_autosnippets = true,
          }

          require("luasnip.loaders.from_vscode").lazy_load {
            require("luasnip").filetype_extend("python", { "django", "pydoc" }),
            require("luasnip").filetype_extend("htmldjango", { "html", "djangohtml" }),
          }
          -- vscode format
          require("luasnip.loaders.from_vscode").lazy_load()
          require("luasnip.loaders.from_vscode").lazy_load { paths = vim.g.vscode_snippets_path or "" }

          -- snipmate format
          require("luasnip.loaders.from_snipmate").load()
          require("luasnip.loaders.from_snipmate").lazy_load { paths = vim.g.snipmate_snippets_path or "" }

          -- lua format
          require("luasnip.loaders.from_lua").load()
          require("luasnip.loaders.from_lua").lazy_load { paths = vim.g.lua_snippets_path or "" }

          vim.api.nvim_create_autocmd("InsertLeave", {
            callback = function()
              if
                require("luasnip").session.current_nodes[vim.api.nvim_get_current_buf()]
                and not require("luasnip").session.jump_active
              then
                require("luasnip").unlink_current()
              end
            end,
          })
        end,
      },

      -- autopairing of (){}[] etc
      {
        "windwp/nvim-autopairs",
        opts = {
          fast_wrap = {},
          disable_filetype = { "TelescopePrompt", "vim" },
        },
        config = function(_, opts)
          require("nvim-autopairs").setup {
            check_ts = true, -- treesitter integration
            disable_filetype = { "TelescopePrompt", "vim" },
            ts_config = {
              lua = { "string", "source" },
              javascript = { "string", "template_string" },
              java = false,
            },
            fast_wrap = {
              map = "<M-e>",
              chars = { "{", "[", "(", '"', "'" },
              pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
              offset = 0, -- Offset from pattern match
              end_key = "$",
              keys = "qwertyuiopzxcvbnmasdfghjkl",
              check_comma = true,
              highlight = "PmenuSel",
              highlight_grey = "LineNr",
            },
          }

          -- setup cmp for autopairs
          local cmp_autopairs = require "nvim-autopairs.completion.cmp"
          require("cmp").event:on("confirm_done", cmp_autopairs.on_confirm_done())
        end,
      },

      -- cmp sources plugins
      {
        "saadparwaiz1/cmp_luasnip",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-path",
        "hrsh7th/cmp-cmdline",
        {
          "saecki/crates.nvim",
          event = "BufRead Cargo.toml",
          config = function()
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
          end,
        },
      },
    },
    opts = function()
      return require "plugins.configs.cmp"
    end,
    config = function(_, opts)
      require("cmp").setup(opts)
      require("cmp").setup.cmdline({ "/", "?" }, {
        sources = {
          { name = "buffer", keyword_length = 1 },
        },
      })
      require("cmp").setup.cmdline(":", {
        sources = {
          { name = "path" },
          { name = "nvim_lua" },
          { name = "cmdline", keyword_length = 2 },
        },
      })
    end,
  },
  ----------------------------------------------------------
  -- Notification
  {
    "rcarriga/nvim-notify",
    event = "VeryLazy",
    config = function()
      dofile(vim.g.base46_cache .. "notify")
      require("notify").setup {
        level = 2,
        minimum_width = 50,
        render = "default",
        stages = "fade_in_slide_out",
        timeout = 3000,
        top_down = true,
      }
    end,
  },
  -------------------------------------------------------------------------------

  {
    {
      "numToStr/Comment.nvim",
      event = { "BufRead", "BufNewFile" },

      init = function()
        require("core.utils").load_mappings "comment"
      end,
      config = function()
        require("Comment").setup {
          -- ignores empty lines
          ignore = "^$",
          pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
        }
      end,
      dependencies = "JoosepAlviste/nvim-ts-context-commentstring",
    },
  },
  -----------------------------------------------------------------
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup {
        -- Configuration here, or leave empty to use defaults
      }
    end,
  },
  -----------------------------------------------------------------
  -- file managing , picker etc
  {
    "nvim-neo-tree/neo-tree.nvim",
    enabled = true,
    branch = "v3.x",
    dependencies = {
      "MunifTanjim/nui.nvim",
    },
    event = "VeryLazy",
    init = function()
      require("core.utils").load_mappings "Neotree"
    end,

    config = function()
      require "plugins.configs.neo-tree"
    end,
  },
  -----------------------------------------------------------------
  {
    "folke/trouble.nvim",
    enabled = true,
    cmd = { "Trouble", "TroubleToggle" },
    opts = require "plugins.configs.trouble",
  },
  -----------------------------------------------------------------
  {

    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-project.nvim",
      "debugloop/telescope-undo.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    cmd = "Telescope",

    init = function()
      require("core.utils").load_mappings "telescope"
    end,

    config = function()
      require "plugins.configs.telescope_finder"
    end,
  },
  -----------------------------------------------------------------
  -- Only load whichkey after all the gui
  {

    "folke/which-key.nvim",
    enabled = true,
    event = "VeryLazy",

    cmd = "WhichKey",
    config = function()
      require "plugins.configs.which-key"
    end,
  },

  -----------------------------------------------------------------
  {
    "rcarriga/nvim-dap-ui",
    init = function()
      require("core.utils").load_mappings "Dap"
    end,
    dependencies = {
      {
        "mfussenegger/nvim-dap",
        config = function()
          require "plugins.configs.dap_adapters.setup"
        end,
      },
      {
        "theHamsta/nvim-dap-virtual-text",
        enabled = true,
        config = function()
          require "plugins.configs.dap_adapters.dab_virt_text"
        end,
      },
    },
    opts = require "plugins.configs.dap_adapters.dap_ui",
  },
}

local config = require("core.utils").load_config()

if #config.plugins > 0 then
  table.insert(default_plugins, { import = config.plugins })
end

require("lazy").setup(default_plugins, config.lazy_nvim)
