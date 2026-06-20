-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

config.use_fancy_tab_bar = false

local function palette_for(appearance)
  local is_dark = appearance:find('Dark') ~= nil
  if is_dark then
    return {
      color_scheme    = 'Modus-Vivendi',
      tabline_theme   = 'Modus-Vivendi',
      bar_bg          = '#1e1e1e',
      bar_hover_bg    = '#2a2a2a',
      fg_dim          = '#a8a8a8',
      fg_bright       = '#ffffff',
      active_tab_bg   = '#000000',
    }
  end
  return {
    color_scheme    = 'Modus-Operandi',
    tabline_theme   = 'Modus-Operandi',
    bar_bg          = '#e0e0e0',
    bar_hover_bg    = '#d0d0d0',
    fg_dim          = '#595959',
    fg_bright       = '#000000',
    active_tab_bg   = '#ffffff',
  }
end

local function tab_bar_colors(palette)
  return {
    background = palette.bar_bg,
    new_tab = { bg_color = palette.bar_bg, fg_color = palette.fg_bright },
    new_tab_hover = { bg_color = palette.bar_hover_bg, fg_color = palette.fg_bright, italic = false },
  }
end

local function tabline_overrides(palette)
  return {
    normal_mode = {
      c = { fg = palette.fg_dim, bg = palette.bar_bg },
    },
    tab = {
      active = { fg = palette.fg_bright, bg = palette.active_tab_bg },
      inactive = { fg = palette.fg_dim, bg = palette.bar_bg },
      inactive_hover = { fg = palette.fg_bright, bg = palette.bar_hover_bg },
    },
  }
end

local initial_palette = palette_for(wezterm.gui.get_appearance())
config.color_scheme = initial_palette.color_scheme
config.colors = { tab_bar = tab_bar_colors(initial_palette) }
-- This is where you actually apply your config choices.

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size.
config.font_size = 14

config.font = wezterm.font(
    'Berkeley Mono Variable Z8XX46Z7',
    { weight = 'Regular' }
)

local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")

tabline.setup({
  options = {
    icons_enabled = true,
    theme = initial_palette.tabline_theme,
    tabs_enabled = true,
    section_separators = '',
    component_separators= '',
    tab_separators = '',
    theme_overrides = tabline_overrides(initial_palette),
  },
  sections = {
    tabline_a = {},
    tabline_b = {},
    tabline_c = {},
    tab_active = {
      { 'index', padding = { left = 2, right = 1 } },
      { 'parent', padding = 0 },
      '/',
      { 'cwd', padding = { left = 0, right = 2 } },
      { 'zoomed', padding = 0 },
    },
    tab_inactive = {
      { 'index', padding = { left = 2, right = 1 } },
      { 'process', padding = { left = 0, right = 2 } },
    },
    tabline_x = { 'ram', 'cpu' },
    tabline_y = {},
    tabline_z = {},
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

  -- Emacs-like keybinds
  -- Option-Left/Right for word navigation
  { key = 'b', mods = 'OPT', action = wezterm.action.SendString '\x1bb' },
  { key = 'f', mods = 'OPT', action = wezterm.action.SendString '\x1bf' },
  -- Ctrl+K for kill line
  {
      key = 'k', mods = 'CTRL', action = wezterm.action.SendKey {
      key = 'U',
      mods = 'CTRL'
    },
  },
  -- M-backspace (Delete a Word backwards)
  { key = 'Backspace',  mods = 'ALT', action = wezterm.action.SendKey({ key = 'w', mods = 'CTRL' }) }
}

config.enable_scroll_bar = false

local function sync_appearance(window)
  if not window then return end
  local palette = palette_for(window:get_appearance())
  local overrides = window:get_config_overrides() or {}
  if overrides.color_scheme ~= palette.color_scheme then
    overrides.color_scheme = palette.color_scheme
    overrides.colors = { tab_bar = tab_bar_colors(palette) }
    window:set_config_overrides(overrides)
    tabline.set_theme(palette.tabline_theme, tabline_overrides(palette))
  end
end

wezterm.on('window-config-reloaded', sync_appearance)
wezterm.on('window-focus-changed', sync_appearance)

-- Finally, return the configuration to wezterm:
return config
