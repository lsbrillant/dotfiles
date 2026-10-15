return {
  'ellisonleao/gruvbox.nvim',
  priority = 1000,
  config = function()
    -- vim.g.gruvbox_transparent_mode = bg_transparent
    require('gruvbox').setup {
      transparent_mode = true,
    }
    vim.cmd [[colorschem gruvbox]]

    local bg_transparent = true

    local toggle_transparency = function()
      bg_transparent = not bg_transparent
      require('gruvbox').setup {
        transparent_mode = bg_transparent,
      }
      -- vim.g.gruvbox_transparent_mode = bg_transparent
      vim.cmd [[colorscheme gruvbox]]
      -- set_menu_border_transparency()
    end

    vim.keymap.set('n', '<leader>bg', toggle_transparency, { noremap = true, silent = true })
  end,
}
