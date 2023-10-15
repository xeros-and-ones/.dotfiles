local M = {
  "nvim-neo-tree/neo-tree.nvim",
  enabled = true,
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    "MunifTanjim/nui.nvim",
  },
  event = "VeryLazy",
}

function M.config()
  vim.cmd [[ let g:neo_tree_remove_legacy_commands = 1 ]]

  require("neo-tree").setup {
    close_if_last_window = true,
    popup_border_style = "single",
    enable_git_status = true,
    enable_modified_markers = true,
    enable_diagnostics = true,
    sort_case_insensitive = true,
    default_component_configs = {
      indent = {
        indent_size = 2,
        padding = 1,
        with_markers = true,
        with_expanders = true,
      },
      modified = {
        symbol = " ",
        highlight = "NeoTreeModified",
      },
      icon = {
        default = "",
        folder_closed = "",
        folder_open = "",
        folder_empty = "",
        folder_empty_open = "",
      },
      git_status = {
        symbols = {
          -- Change type
          added = "",
          deleted = "",
          modified = "",
          renamed = "➜",
          -- Status type
          untracked = "",
          ignored = "◌",
          unstaged = "✗",
          staged = "✓",
          conflict = "",
        },
      },
    },
    window = {
      position = "left",
      width = 35,
      popup = {
        position = { col = "2", row = "3" },
        size = function(state)
          local root_name = vim.fn.fnamemodify(state.path, ":~")
          local root_len = string.len(root_name) + 4
          return {
            width = math.max(root_len, 50),
            height = vim.o.lines - 6,
          }
        end,
      },
      mappings = {
        ["<space>"] = false, -- disable space until we figure out which-key disabling
        ["<cr>"] = "open",
        ["p"] = { "toggle_preview", config = { use_float = true } },
        ["s"] = "open_split",
        ["v"] = "open_vsplit",
        -- ["s"] = "split_with_window_picker",
        -- ["v"] = "vsplit_with_window_picker",
        -- ["<cr>"] = "open_drop",
        -- ["t"] = "open_tab_drop",
        ["w"] = "open_with_window_picker",
        ["d"] = "delete",
        ["r"] = "rename",
        H = "prev_source",
        L = "next_source",
        ["a"] = {
          "add",
          config = {
            show_path = "relative",
          },
        },
        ["A"] = {
          "add_directory",
          config = {
            show_path = "relative",
          },
        },
        ["c"] = {
          "copy",
          config = {
            show_path = "relative",
          },
        },
        ["m"] = {
          "move",
          config = {
            show_path = "relative",
          },
        },
      },
    },
    sources = { "filesystem", "buffers", "git_status", "document_symbols" },
    filesystem = {
      bind_to_cwd = true, -- true creates a 2-way binding between vim's cwd and neo-tree's root
      cwd_target = {
        sidebar = "tab", -- match this to however cd is set elsewhere (tab, window, global)
      },
      filtered_items = {
        visible = false, -- when true, they will just be displayed differently than normal items
        hide_dotfiles = false,
        hide_gitignored = false,
        hide_by_name = {
          ".DS_Store",
          "thumbs.db",
          "node_modules",
          "__pycache__",
        },
      },
      follow_current_file = {
        enabled = true,
      },
      use_libuv_file_watcher = true, -- This will use the OS level file watchers to detect changes
      window = {
        mappings = {
          ["<bs>"] = "navigate_up",
          ["."] = "set_root",
          ["H"] = "toggle_hidden",
          ["/"] = "fuzzy_finder",
          ["D"] = "fuzzy_finder_directory",
          ["f"] = "filter_on_submit",
          ["<c-x>"] = "clear_filter",
          ["[g"] = "prev_git_modified",
          ["]g"] = "next_git_modified",
        },
      },
    },
    modified = {
      symbol = "[+]",
      highlight = nil,
    },
    name = {
      trailing_slash = false,
      use_git_status_colors = true,
      highlight = "NeoTreeFileName",
    },
    buffers = {
      follow_current_file = {
        enabled = true,
      },
      group_empty_dirs = false,
    },
    git_status = {
      window = {
        position = "float",
      },
      symbols = {
        -- Change type
        added = "",
        deleted = "",
        modified = "",
        renamed = "➜",
        -- Status type
        untracked = "",
        ignored = "◌",
        unstaged = "✗",
        staged = "✓",
        conflict = "",
      },
    },
    event_handlers = {
      {
        event = "neo_tree_buffer_enter",
        handler = function(_)
          vim.opt_local.signcolumn = "auto"
        end,
      },
      {
        event = "neo_tree_window_after_open",
        handler = function(args)
          if args.position == "left" or args.position == "right" then
            vim.cmd "wincmd ="
          end
        end,
      },
      {
        event = "neo_tree_window_after_close",
        handler = function(args)
          if args.position == "left" or args.position == "right" then
            vim.cmd "wincmd ="
          end
        end,
      },
    },
  }
end

return M
