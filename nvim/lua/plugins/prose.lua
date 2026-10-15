return {
  {
    'junegunn/limelight.vim',
    config = function()
      vim.g.limelight_conceal_ctermfg = 00
    end,
  },
  {
    'junegunn/goyo.vim',
    config = function()
      on_enter = function()
        vim.cmd [[Limelight]]
      end

      on_exit = function()
        vim.cmd [[Limelight!]]
      end

      vim.api.nvim_create_autocmd('User', {
        desc = 'Run on_enter Goyo',
        pattern = 'GoyoEnter',
        callback = function(ev)
          on_enter()
        end,
      })
      vim.api.nvim_create_autocmd('User', {
        desc = 'Run on_enter Goyo',
        pattern = 'GoyoLeave',
        callback = function(ev)
          on_exit()
        end,
      })
    end,
  },
}
