# dotup
A CLI tool to help symlink your dotfiles into place.

## Installation
```
$ cargo install --git https://github.com/diogo464/dotup
```

## Usage
Example file hierarchy with dotfiles.
```
  configs
    nvim/
      init.vim
      lua/
	setup.lua
    alacritty/
      alacritty.yml
    bash/
      .bashrc
    scripts/
      script1.sh
      script2.sh
      script3.sh
```

### dotup init
Running `dotup init` will create an empty depot file in the current directory(by default).
```
$ dotup init
```
A new file, `depot.toml`, should have been created in the current directory.

### dotup link
The `link` subcommand can be used to tell where a file should be linked when installed.
Running `dotup link nvim .config/nvim` will create two new links:
+ `nvim/init.vim` -> `.config/nvim/init.vim`
+ `nvim/lua/setup.lua` -> `.config/nvim/lua/setup.lua`
This subcommand will, by default, only link files. If a directory is passed as argument then it will recursively link all files under that directory.

To link a directory use the flag `--directory`.
```
$ dotup link --directory scripts .scripts
```
This will create a new link
+ `scripts` -> `.scripts`

### dotup unlink
The `unlink` subcommand unlinks files.
```
$ dotup unlink nvim/lua/setup.lua
```
will remove the link `nvim/lua/setup.lua` -> `.config/nvim/lua/setup.lua`.

### dotup install
The `install` subcommand creates symlinks.
Like the `link` subcommand passing a directory as argument will recursively install anything under it, files and directories.

By default install will use the home directory as the install-base but this can be changed with the `--install-base <path>` parameter.
```
$ dotup install nvim
```
will create two symlinks
+ `nvim/init.vim` -> `$HOME/.config/nvim/init.vim`
+ `nvim/lua/setup.lua` -> `$HOME/.config/nvim/lua/setup.lua`

```
$ dotup install scripts
```
will create one symlink
+ `scripts` -> `$HOME/.scripts`
Any new scripts added to `$HOME/.scripts` will be created at `configs/scripts`.

### dotup uninstall
The `uninstall` subcommand removes the symlinks.
This will only remove symlinks if they were pointing to the correct file.

