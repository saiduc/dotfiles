-- Bootstrap packer package manager
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end


return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'
    use 'Mofiqul/dracula.nvim'
    use 'b3nj5m1n/kommentary'
    use 'nvim-lualine/lualine.nvim'
    use 'windwp/nvim-autopairs'
    use 'github/copilot.vim'
    use 'lervag/vimtex'
    use 'akinsho/toggleterm.nvim'
    use {'neoclide/coc.nvim', branch='release', run={':CocInstall coc-pyright', ':CocInstall coc-vimtex'}}
    use {'kyazdani42/nvim-tree.lua', requires={'kyazdani42/nvim-web-devicons'}}
    use {'alvarosevilla95/luatab.nvim', requires='kyazdani42/nvim-web-devicons'}


-- Put this at the end after all plugins
-- Automatically set up your configuration after cloning packer.nvim
function file_exists(file)
  local isok, errstr, errcode = os.rename(file, file)
  if isok == nil then
     if errcode == 20 then 
        return true
     end
     return false
  end
  return true
end
function dir_exists(path) return file_exists(path .. "/") end
local ran_sync = dir_exists('/home/saipandian/.config/nvim/plugin')
if not ran_sync then 
    require('packer').sync()
end


-- Plugin configurations
require('nvim-web-devicons').setup{default=true;}

require('lualine').setup{}

require('nvim-autopairs').setup{}

require('nvim-tree').setup{}

vim.g['tex_flavor'] = 'latex'
vim.g['vimtex_viewer_method'] = 'skim'

require('luatab').setup{}


require("toggleterm").setup{open_mapping = [[<C-t>]]}


end)
