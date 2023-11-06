local cmp = require "cmp"
local luasnip = require "luasnip"

dofile(vim.g.base46_cache .. "cmp")

local cmp_ui = require("core.utils").load_config().ui.cmp
local cmp_style = cmp_ui.style

local field_arrangement = {
  atom = { "kind", "abbr", "menu" },
  atom_colored = { "kind", "abbr", "menu" },
}

local formatting_style = {
  -- default fields order i.e completion word + item.kind + item.kind icons
  fields = field_arrangement[cmp_style] or { "abbr", "kind", "menu" },

  format = function(_, item)
    local icons = require "nvchad.icons.lspkind"
    local icon = (cmp_ui.icons and icons[item.kind]) or ""

    if cmp_style == "atom" or cmp_style == "atom_colored" then
      icon = " " .. icon .. " "
      item.menu = cmp_ui.lspkind_text and "   (" .. item.kind .. ")" or ""
      item.kind = icon
    else
      icon = cmp_ui.lspkind_text and (" " .. icon .. " ") or icon
      item.kind = string.format("%s %s", icon, cmp_ui.lspkind_text and item.kind or "")
    end

    return item
  end,
}
local has_words_before = function()
  if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
    return false
  end
  ---@diagnostic disable-next-line: deprecated
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match "^%s*$" == nil
end
local function border(hl_name)
  return {
    { "╭", hl_name },
    { "─", hl_name },
    { "╮", hl_name },
    { "│", hl_name },
    { "╯", hl_name },
    { "─", hl_name },
    { "╰", hl_name },
    { "│", hl_name },
  }
end

local options = {
  completion = {
    completeopt = "menu,menuone",
  },

  window = {
    completion = {
      side_padding = (cmp_style ~= "atom" and cmp_style ~= "atom_colored") and 1 or 0,
      winhighlight = "Normal:CmpPmenu,CursorLine:CmpSel,Search:PmenuSel",
      scrollbar = false,
    },
    documentation = {
      border = border "CmpDocBorder",
      winhighlight = "Normal:CmpPmenu",
    },
  },
  experimental = {
    ghost_text = true,
    native_menu = false,
  },
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },

  formatting = formatting_style,

  mapping = {
    ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select }, { "i", "c" }),
    ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select }, { "i", "c" }),
    ["<C-k>"] = cmp.mapping(cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert }, { "i", "c" }),
    ["<C-j>"] = cmp.mapping(cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert }, { "i", "c" }),
    ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
    ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
    ["<c-space>"] = cmp.mapping {
      i = cmp.mapping.complete(),
      c = function(_) -- fallback
        if cmp.visible() then
          if not cmp.confirm { select = true } then
            return
          end
        else
          cmp.complete()
        end
      end,
    },
    ["<C-y>"] = cmp.mapping(cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Insert, select = true }, { "i", "c" }),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ["<Tab>"] = cmp.mapping(function(fallback)
      -- if cmp.visible() then
      --     cmp.select_next_item()
      if luasnip.expandable() then
        luasnip.expand()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif require("neogen").jumpable(1) then
        require("neogen").jump_next()
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
      -- if cmp.visible() then
      --     cmp.select_prev_item()
      if luasnip.jumpable(-1) then
        luasnip.jump(-1)
      elseif require("neogen").jumpable(-1) then
        require("neogen").jump_prev()
      else
        fallback()
      end
    end, {
      "i",
      "s",
    }),
  },
  sources = {
    { name = "nvim_lsp" },
    { name = "luasnip", option = { show_autosnippets = true } },
    { name = "buffer" },
    { name = "nvim_lua" },
    { name = "path" },
  },
}

if cmp_style ~= "atom" and cmp_style ~= "atom_colored" then
  options.window.completion.border = border "CmpBorder"
end

return options
