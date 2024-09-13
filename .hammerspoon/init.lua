-- New Emacs windows open with focus
-- See https://github.com/d12frosted/homebrew-emacs-plus/discussions/636

-- Importing necessary modules from Hammerspoon
local application = require 'hs.application'
local window = require 'hs.window'

-- Setting up a filter for Emacs windows
emacsFilter = hs.window.filter.new('Emacs')

-- Function to focus on the newest Emacs window
function focusNewEmacsWindow()
   -- Getting all Emacs windows, sorted by the time they were created (newest first)
    local windows = emacsFilter:getWindows(hs.window.filter.sortByCreatedLast)
    -- If there are any Emacs windows, focus on the newest one
    if #windows > 0 then
        windows[1]:focus()
    end
end

-- Subscribing to window creation events for Emacs windows
-- Calls focusNewEmacsWindow function each time a new Emacs window is created
emacsFilter:subscribe(hs.window.filter.windowCreated, focusNewEmacsWindow)
