require("which-key").setup(astronvim.user_plugin_opts("plugins.which-key", {
  plugins = {
    spelling = { enabled = true },
    presets = { operators = false },
  },
  window = {
    border = "rounded",
    padding = { 0, 0, 0, 0 },
  },
  disable = { filetypes = { "TelescopePrompt" } },
}))
