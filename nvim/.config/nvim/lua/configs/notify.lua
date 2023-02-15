local notify = require "notify"
notify.setup(
    astronvim.user_plugin_opts(
        "plugins.notify",
        { stages = "fade_in_slide_out", background_colour = "#000000", opacity = 20, timeout = 3000 }
    )
)
vim.notify = notify
