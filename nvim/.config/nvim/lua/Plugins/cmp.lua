local M = {
  "hrsh7th/nvim-cmp",
  enabled = true,
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "onsails/lspkind.nvim",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-emoji",
    "saadparwaiz1/cmp_luasnip",
    "L3MON4D3/LuaSnip",
    "rafamadriz/friendly-snippets",
  },

  event = { "InsertEnter" },
}

local has_words_before = function()
  if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
    return false
  end
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match "^%s*$" == nil
end

function M.config()
  local cmp = require "cmp"
  local lspkind = require "lspkind"
  local luasnip = require "luasnip"
  local mapping = {
    ["<Up>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select },
    ["<Down>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
    ["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
    ["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
    ["<C-k>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
    ["<C-j>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
    ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
    ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
    ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    ["<C-y>"] = cmp.config.disable,
    ["<C-e>"] = cmp.mapping {
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    },
    ["<CR>"] = cmp.mapping.confirm { select = false },
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expandable() then
        luasnip.expand()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end, {
      "i",
      "s",
    }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, {
      "i",
      "s",
    }),
  }

  cmp.setup {
    preselect = cmp.PreselectMode.None,
    completion = {
      completeopt = "menu,menuone,noinsert, noselect",
    },
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    experimental = {
      ghost_text = true,
    },
    window = {
      completion = cmp.config.window.bordered {
        winhighlight = "Normal:Normal,FloatBorder:FloatBorder,Search:None",
        border = "single",
      },
      documentation = cmp.config.window.bordered {
        winhighlight = "Normal:Normal,FloatBorder:FloatBorder,Search:None",
        border = "single",
      },
    },
    formatting = {
      -- fields = { "kind", "abbr", "menu" },
      -- format = function(entry, vim_item)
      --   vim_item.kind = string.format("%s %s", lspkind[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
      --   vim_item.menu = ("")
      --     -- ({
      --     -- 	nvim_lsp = "[LSP]",
      --     -- 	luasnip = "[Snippet]",
      --     -- 	buffer = "[Buffer]",
      --     -- 	path = "[Path]",
      --     -- })
      --     [entry.source.name]
      --   vim_item.dup = 0
      --   return vim_item
      -- end,
      ----------------------------------------------------
      fields = { "kind", "abbr", "menu" },
      format = function(entry, vim_item)
        local kind = lspkind.cmp_format { mode = "symbol_text", maxwidth = 50 }(entry, vim_item)
        local strings = vim.split(kind.kind, "%s", { trimempty = false })
        kind.kind = " " .. strings[1] .. " "
        kind.menu = "    " .. strings[2] .. ""

        return kind
      end,
    },
    confirm_opts = {
      behavior = cmp.ConfirmBehavior.Replace,
      select = false,
    },
    mapping = mapping,
    sources = {
      { name = "nvim_lsp", priority = 500 },
      { name = "nvim_lsp_signature_help" },
      { name = "luasnip", priority = 300, option = { show_autosnippets = true } },
      { name = "nvim_lua", ft = "lua" },
      { name = "path" },
      { name = "crates" },
    },
  }

  -- FIXME: completion not showing up for this, or is not navigatable
  cmp.setup.cmdline({ "/", "?" }, {
    mapping = mapping,
    sources = {
      { name = "buffer", keyword_length = 1 },
    },
  })

  cmp.setup.cmdline(":", {
    mapping = mapping,
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      {
        name = "cmdline",
        option = {
          ignore_cmds = { "Man", "!" },
        },
      },
    }),
  })

  cmp.setup.filetype({ "sql", "mysql", "plsql" }, { sources = { { name = "vim-dadbod-completion" } } })
end

return M
