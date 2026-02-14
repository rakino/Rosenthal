local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- Use Xwayland in the LiveCD so that we can change keyboard layout when
-- WezTerm is running.
config.enable_wayland = false

return config
