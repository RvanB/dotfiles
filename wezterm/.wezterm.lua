-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

config.use_fancy_tab_bar = false

local selected_theme = 'rvb3'

local palettes = {
  rvb1 = {
    color_scheme  = 'rvb1',
    bar_bg        = '#262626',
    bar_hover_bg  = '#5f5faf',
    fg_dim        = '#808080',
    fg_bright     = '#ffffff',
    active_tab_bg = '#000000',
    colors = {
      foreground = '#ffffff',
      background = '#000000',
      cursor_bg = '#dadada',
      cursor_fg = '#000000',
      cursor_border = '#dadada',
      selection_bg = '#5f5faf',
      selection_fg = '#ffffff',
      ansi = { '#000000', '#ff0000', '#00af5f', '#f0a050', '#00afff', '#af5f5f', '#5fffaf', '#808080' },
      brights = { '#595959', '#ff5f5f', '#00ff5f', '#ffaf5f', '#5fcfff', '#af5faf', '#afffaf', '#ffffff' },
    },
  },
  rvb2 = {
    color_scheme  = 'rvb2',
    bar_bg        = '#27282a',
    bar_hover_bg  = '#667a8b',
    fg_dim        = '#a7b2aa',
    fg_bright     = '#eeeeed',
    active_tab_bg = '#010202',
    colors = {
      foreground = '#eeeeed',
      background = '#010202',
      cursor_bg = '#eeeeed',
      cursor_fg = '#010202',
      cursor_border = '#eeeeed',
      selection_bg = '#eeeeed',
      selection_fg = '#010202',
      ansi = { '#010202', '#cb4c52', '#63c174', '#998375', '#667a8b', '#b568a6', '#74b8b7', '#a7b2aa' },
      brights = { '#a7b2aa', '#ef6f6f', '#d3e3b1', '#fbf695', '#829aaa', '#d184c2', '#77c5c3', '#eeeeed' },
    },
  },
  rvb3 = {
    color_scheme  = 'rvb3',
    bar_bg        = '#171717',
    bar_hover_bg  = '#fafafa',
    bar_hover_fg  = '#171717',
    fg_dim        = '#fafafa',
    fg_bright     = '#fafafa',
    active_tab_bg = '#fafafa',
    active_tab_fg = '#171717',
    colors = {
      foreground = '#171717',
      background = '#fafafa',
      cursor_bg = '#171717',
      cursor_fg = '#fafafa',
      cursor_border = '#171717',
      selection_bg = '#171717',
      selection_fg = '#fafafa',
      ansi = { '#171717', '#171717', '#171717', '#171717', '#171717', '#171717', '#171717', '#171717' },
      brights = { '#666666', '#171717', '#171717', '#171717', '#171717', '#171717', '#171717', '#171717' },
    },
  },
}

local function palette_for(theme)
  return assert(palettes[theme], 'Unknown terminal theme: ' .. theme)
end

local function tab_bar_colors(palette)
  return {
    background = palette.bar_bg,
    new_tab = { bg_color = palette.bar_bg, fg_color = palette.fg_bright },
    new_tab_hover = { bg_color = palette.bar_hover_bg, fg_color = palette.bar_hover_fg or palette.fg_bright, italic = false },
  }
end

local function tabline_overrides(palette)
  return {
    normal_mode = {
      c = { fg = palette.fg_dim, bg = palette.bar_bg },
    },
    tab = {
      active = { fg = palette.active_tab_fg or palette.fg_bright, bg = palette.active_tab_bg },
      inactive = { fg = palette.fg_dim, bg = palette.bar_bg },
      inactive_hover = { fg = palette.bar_hover_fg or palette.fg_bright, bg = palette.bar_hover_bg },
    },
  }
end

local initial_palette = palette_for(selected_theme)
local terminal_colors = initial_palette.colors

config.color_schemes = {
  rvb1 = palettes.rvb1.colors,
  rvb2 = palettes.rvb2.colors,
  rvb3 = palettes.rvb3.colors,
}
config.color_scheme = initial_palette.color_scheme
config.colors = { tab_bar = tab_bar_colors(initial_palette) }
-- This is where you actually apply your config choices.

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size.
config.font_size = 14

config.font = wezterm.font(
    'SF Mono',
    { weight = 'Medium' }
)

local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")

tabline.setup({
  options = {
    icons_enabled = true,
    theme = terminal_colors,
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

-- Finally, return the configuration to wezterm:
return config
