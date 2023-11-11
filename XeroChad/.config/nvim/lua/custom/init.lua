require("custom.core.options")
require("custom.core.autocommands")
require("custom.core.utilities")

-----------------------------------------------------------
if vim.g.neovide then
	vim.opt.guifont = "FiraCode Nerd Font:h11"
	vim.g.neovide_transparency = 0.8
	-- vim.g.neovide_floating_blur_amount_x = 2.0
	-- vim.g.neovide_floating_blur_amount_y = 2.0
	-- vim.g.neovide_refresh_rate = 100
	-- vim.g.neovide_refresh_rate_idle = 5
	local function toggle_transparency()
		if vim.g.neovide_transparency == 1.0 then
			vim.cmd("let g:neovide_transparency=0.8")
		else
			vim.cmd("let g:neovide_transparency=1.0")
		end
	end

	local function toggle_fullscreen()
		if vim.g.neovide_fullscreen == false then
			vim.cmd("let g:neovide_fullscreen=v:true")
		else
			vim.cmd("let g:neovide_fullscreen=v:false")
		end
	end

	vim.keymap.set("n", "<F11>", toggle_fullscreen, { silent = true })
	vim.keymap.set("n", "<F10>", toggle_transparency, { silent = true })
end
