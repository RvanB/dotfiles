-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices.

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size and color scheme.
config.font_size = 14
config.color_scheme = 'Modus-Operandi'
config.use_fancy_tab_bar = false
config.font = wezterm.font(
    'Berkeley Mono Variable Z8XX46Z7',
    { weight = 'Regular' }
)

config.enable_scroll_bar = false

-- Finally, return the configuration to wezterm:
return config
