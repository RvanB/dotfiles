-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

config.use_fancy_tab_bar = false

config.colors = {
  tab_bar = {
    -- The color of the strip that goes along the top of the window
    -- (does not apply when fancy tab bar is in use)
    background = '#ffffff',
    new_tab = {
        bg_color = '#ffffff',
        fg_color = '#000000',
    },
    new_tab_hover = {
        bg_color = '#ffffff',
        fg_color = '#000000',
        italic = true,
    },
  },
}
-- This is where you actually apply your config choices.

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size and color scheme.
config.font_size = 14
config.color_scheme = 'Modus-Operandi'

config.font = wezterm.font(
    'Berkeley Mono Variable Z8XX46Z7',
    { weight = 'Regular' }
)

local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")

tabline.setup({
  options = {
    icons_enabled = true,
    theme = "Modus-Operandi",
    tabs_enabled = true,
    section_separators = '',
    component_separators= '',
    tab_separators = '',
  },
  sections = {
    tabline_a = {},
    tabline_b = {},
    tabline_c = {},
    tab_active = {
      'index',
      { 'parent', padding = 0 },
      '/',
      { 'cwd', padding = { left = 0, right = 1 } },
      { 'zoomed', padding = 0 },
    },
    tab_inactive = { 'index', { 'process', padding = { left = 0, right = 1 } } },
    tabline_x = { 'ram', 'cpu' },
    tabline_y = { 'datetime' },
    tabline_z = { 'domain' },
  },
  extensions = {},
})


config.keys = {
  -- Navigate panes with ALT + hjkl
  { key = 'h', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Left' },
  { key = 'j', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Down' },
  { key = 'k', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Up' },
  { key = 'l', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Right' },

  -- Visual pane selection (numbers)
  { key = '0', mods = 'CTRL', action = wezterm.action.PaneSelect({ alphabet = '1234567890' }) },
}

config.enable_scroll_bar = false

-- Finally, return the configuration to wezterm:
return config
