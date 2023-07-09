# dmenu-bkmks
_A simple dmenu script that helps you manage your bookmarks in a way that sucks a bit less._

## Installation
### Dependencies:
- [dmenu](https://tools.suckless.org/dmenu/) -- (Dynamic menu for X)
- [yad](https://github.com/v1cont/yad) -- (Yet Another Dialog); Used for help menu and error dialogs
- [SBCL](https://www.sbcl.org/platform-table.html) -- (Common Lisp implementation); needed only for building from source
- [Quicklisp](https://www.quicklisp.org/beta/) -- (A library manager for Common Lisp); needed only for building from source

### Building
Clone the repository with:

`git clone https://github.com/belaja-akacija/bkmks-dmenu.git`

Change into the installed directory:

`cd bkmks-dmenu`

Run:

`make build install`

__That's it!__

To uninstall, run:

`make uninstall`

### Installing Pre-release Binary
Download the pre-release binary.
Go to the directory where you saved the tar.gz file, then,
run:

`tar -xvf bkmks.tar.gz && mv bkmks ~/.local/bin/`

to install the binary.

Make sure you have `~/.local/bin/` in your `$PATH` variable in order for dmenu to find it.


## Usage

add a new bookmark

`bkmks a[dd] <url>`

delete an entry

`bkmks d[el] <selected entry>`

show all bookmarks and go to link in prefered browser

`bkmks c[hg] <selected entry>`

changes the current bookmark category you are in

`bkmks cata[dd] <selected entry>`

adds a new category

`bkmks catd[el] <selected entry>`

deletes a category

`bkmks [ls]`

__Configuration is done by editing the configuration file, located at `/home/user/.config/bkmks/`.__
__*NOTE: In the current version, only dmenu is supported, but in future releases, will support rofi and fzf.*__

If you would prefer to have your bookmarks stored in an alternate locatation, there are variables that can be changed for that. The default is `/home/user/.config/bkmks/files/urls`


## Tips

If you use dwm or another tiling window manager, you might want to set a keybinding to execute `bkmks` to more conveniently access your bookmarks.

Here is what I have in my `config.def.h` for dwm:
```
static const char *bkmksls[]  = { "bkmks", NULL };

static Key keys[] = {
// ...
	{ MODKEY|ShiftMask,             XK_b,      spawn,          {.v = bkmksls } },

//..
}

```

## Notes on progress

✓ Resolved! -- Currently, I am working on refactoring the code to be a bit leaner and less redundant in some places.
I'm having an issue with the config file updating properly in the testing branch, after I have added the `(bkmks-add-category)` and `(bkmks-del-category)` functions.
The functions themselves work fine, but are dependent on the local loaded config global variable to be up to date with the current state of the config file (and subsequently the directory structure of the bookmarks files). Thus, they are not ready to be merged into the main branch quite yet.
