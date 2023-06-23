# bkmks-dmenu
_A simple dmenu script that helps you manage your bookmarks in a way that sucks a bit less._

## Installation
### Dependencies:
- [dmenu](https://tools.suckless.org/dmenu/) -- (Dynamic menu for X)
- [SBCL](https://www.sbcl.org/platform-table.html) -- (Common Lisp implementation)
- [Quicklisp](https://www.quicklisp.org/beta/) -- (A library manager for Common Lisp)
- [yad](https://github.com/v1cont/yad) -- (Yet Another Dialog)

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

`bkmks [ls]`

Configuration is done by directly editing the script.

If you would prefer to have your bookmarks stored in an alternate locatation, there are variables that can be changed for that. The default is `/home/user/.bkmks/urls`


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

