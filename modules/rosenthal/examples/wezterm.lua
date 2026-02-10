-- Keep WezTerm maximized so that the installer won't be resized.
-- https://wezterm.org/config/lua/gui-events/gui-startup.html
local wezterm = require 'wezterm'
local mux = wezterm.mux
local config = {}

wezterm.on('gui-startup', function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

return config
