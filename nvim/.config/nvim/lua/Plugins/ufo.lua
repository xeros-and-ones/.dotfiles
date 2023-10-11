local M = {
  "kevinhwang91/nvim-ufo",
  event = { "BufReadPost", "BufNewFile" },
  enabled = true,
  dependencies = "kevinhwang91/promise-async",
}
function M.config()
  local handler = function(virtText, lnum, endLnum, width, truncate)
    local newVirtText = {}
    local suffix = (" 󰁃 %d --------------------------------"):format(endLnum - lnum)
    local sufWidth = vim.fn.strdisplaywidth(suffix)
    local targetWidth = width - sufWidth
    local curWidth = 0
    for _, chunk in ipairs(virtText) do
      local chunkText = chunk[1]
      local chunkWidth = vim.fn.strdisplaywidth(chunkText)
      if targetWidth > curWidth + chunkWidth then
        table.insert(newVirtText, chunk)
      else
        chunkText = truncate(chunkText, targetWidth - curWidth)
        local hlGroup = chunk[2]
        table.insert(newVirtText, { chunkText, hlGroup })
        chunkWidth = vim.fn.strdisplaywidth(chunkText)
        -- str width returned from truncate() may less than 2nd argument, need padding
        if curWidth + chunkWidth < targetWidth then
          suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
        end
        break
      end
      curWidth = curWidth + chunkWidth
    end
    table.insert(newVirtText, { suffix, "MoreMsg" })
    return newVirtText
  end

  local ftMap = {
    vim = "indent",
    python = { "indent" },
    git = "",
  }

  local keymap = vim.keymap.set
  --Folds
  keymap("n", "zR", require("ufo").openAllFolds, { noremap = false, silent = true, desc = "Open all folds" })
  keymap("n", "zM", require("ufo").closeAllFolds, { noremap = false, silent = true, desc = "Close all folds" })
  keymap("n", "zr", require("ufo").openFoldsExceptKinds, { noremap = false, silent = true, desc = "Open folds" })
  keymap("n", "zm", require("ufo").closeFoldsWith, { noremap = false, silent = true, desc = "Close folds" })
  keymap("n", "zh", function()
    local winid = require("ufo").peekFoldedLinesUnderCursor()
    if not winid then
      -- choose one of coc.nvim and nvim lsp
      vim.lsp.buf.hover()
    end
  end, { noremap = false, silent = true, desc = "Fold preview" })

  require("ufo").setup {
    fold_virt_text_handler = handler,
    open_fold_hl_timeout = 150,
    close_fold_kinds = { "imports", "comment" },

    preview = {
      win_config = {
        border = "rounded",
        winhighlight = "Normal:Folded",
        winblend = 0,
      },
      mappings = {
        scrollU = "<Up>",
        scrollD = "<Down>",
        close = "<esc>",
      },
    },

    provider_selector = function(bufnr, filetype, buftype)
      return ftMap[filetype] or { "treesitter", "indent" }
    end,
  }
end

return M
