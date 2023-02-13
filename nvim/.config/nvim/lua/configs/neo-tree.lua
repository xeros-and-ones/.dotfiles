require("neo-tree").setup(astronvim.user_plugin_opts("plugins.neo-tree", {
  close_if_last_window = true,
  popup_border_style = "rounded",
  enable_diagnostics = true,
  enable_git_status = true,
  source_selector = {
    winbar = true,
    content_layout = "center",
    tab_labels = {
      filesystem = astronvim.get_icon "FolderClosed" .. " File",
      buffers = astronvim.get_icon "DefaultFile" .. " Bufs",
      git_status = astronvim.get_icon "Git" .. " Git",
      diagnostics = astronvim.get_icon "Diagnostic" .. " Diagnostic",
    },
  },
  default_component_configs = {
    indent = {
      indent_size = 2,
      padding = 1,
      with_expanders = true,
    },
    icon = {
      folder_closed = "",
      folder_open = "",
      folder_empty = "",
      default = "",
    },
    git_status = {
      symbols = {
        added = "",
        deleted = "",
        modified = "",
        renamed = "➜",
        untracked = "★",
        ignored = "◌",
        unstaged = "✗",
        staged = "✓",
        conflict = "",
      },
    },
  },
  window = {
    width = 35,
    mappings = {
      ["<space>"] = false, -- disable space until we figure out which-key disabling
      ["o"] = "open",
      ["p"] = { "toggle_preview", config = { use_float = true } },
      -- ["S"] = "open_split",
      -- ["s"] = "open_vsplit",
      ["s"] = "split_with_window_picker",
      ["v"] = "vsplit_with_window_picker",
      ["t"] = "open_tabnew",
      -- ["<cr>"] = "open_drop",
      -- ["t"] = "open_tab_drop",
      ["w"] = "open_with_window_picker",
      ["A"] = "add_directory", -- also accepts the optional config.show_path option like "add".
      ["d"] = "delete",
      ["r"] = "rename",
      H = "prev_source",
      L = "next_source",
    },
  },
  filesystem = {
    filtered_items = {
      visible = false,
      hide_dotfiles = false,
      hide_gitignored = false,
      hide_by_name = {
        ".DS_Store",
        "thumbs.db",
        "node_modules",
        "__pycache__",
      },
    },
    follow_current_file = true,
    hijack_netrw_behavior = "open_current",
    use_libuv_file_watcher = true,
    window = {
      mappings = {
        O = "system_open",
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
    commands = {
      system_open = function(state) astronvim.system_open(state.tree:get_node():get_id()) end,
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
  git_status = {
    symbols = {
      -- Change type
      added = "✚", -- or "✚", but this is redundant info if you use git_status_colors on the name
      modified = "", -- or "", but this is redundant info if you use git_status_colors on the name
      deleted = "✖", -- this can only be used in the git_status source
      renamed = "", -- this can only be used in the git_status source
      -- Status type
      untracked = "",
      ignored = "",
      unstaged = "",
      staged = "",
      conflict = "",
    },
    window = {
      position = "float",
    },
  },
  event_handlers = {
    { event = "neo_tree_buffer_enter", handler = function(_) vim.opt_local.signcolumn = "auto" end },
  },
}))
