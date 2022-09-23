# dotup

CLI tool to symlink configuration files to into their correct place.

Create a `dotup` file in the directory that contains all config files.
```
configs/
	neovim/
		...
	alacritty/
		...
	dotup
```

Populate the config file
```
group desktop {
	include group="neovim"
	include group="alacritty"
}

group neovim {
	link source="neovim/" target=".config/nvim"
}

group alacritty {
	link source="alacritty/" target=".config/alacritty"
}
```

Install the symlinks
```bash
dotup install desktop
```
