local M = {
  "stevearc/aerial.nvim",
  commit = "551a2b679f265917990207e6d8de28018d55f437",
  enabled = true,
  cmd = { "AerialToggle", "AerialOpen", "AerialOpenAll" },
}

M.keys = {
  { "<leader>a", "<cmd>AerialToggle<cr>", desc = "Aerial Outline" },
}

function M.config()
  require("aerial").setup {
    on_attach = function(bufnr)
      vim.keymap.set("n", "[a", "<cmd>AerialPrev<CR>", { desc = "Previous aerial", buffer = bufnr })
      vim.keymap.set("n", "]a", "<cmd>AerialNext<CR>", { desc = "Next aerial", buffer = bufnr })
    end,
    backends = { "lsp", "treesitter", "markdown", "man" },
    layout = {
      default_direction = "right",
      preserve_equality = true,
      max_width = 0.3,
      min_width = 0.2,
    },
    float = {
      --   cursor - Opens float on top of the cursor
      --   editor - Opens float centered in the editor
      --   win    - Opens float centered in the window
      relative = "win",
      border = "rounded",
      max_height = 0.9,
      height = nil,
      min_height = { 8, 0.1 },
    },
    lazy_load = true,
    close_automatic_events = { "unsupported" },
    close_on_select = true,
    highlight_on_hover = true,
    show_guides = true,
  }
end

return M
