# waymonad

We all love tiling window managers and most of all we love [xmonad](https://github.com/xmonad/xmonad).
As you may know, [wayland](https://wayland.freedesktop.org/) is trying to replace X11.

This brings problems for us, since xmonad will not work with the new architecture.

This project is intended to provide a wayland based desktop which shares the ideals and experience from xmonad.

## [WIP]: This is work in Progress.
### There's no **screenlocker**/bar/background application/quality of life

Until `surface-layers` is in, these won't be implemented.
I have no idea when that happens, so until then expect a rather barebones experience (I may implement background/simple locking if it takes too long).

If you need to lock your session: [vlock](https://linux.die.net/man/1/vlock) or [physlock](https://github.com/muennich/physlock) can help you.
This is a bit of a hassle (and not done automagically :/) but it works.

### "Minor" things

* I don't have a floating layer yet. Though override redirect works.
* dmenu doesn't work! (the grab isn't supported on wayland)
* There is barely any documentation so unless you are familiar with Haskell (and preferably xmonad) this may be rather hard to get comfortable with.

-----
# "Install" (compile and execute locally):

To test this, you need `wlroots` installed.
This currently only builds with the `new-build` feature of cabal-install. `stack` is (not yet) supported.

 * git clone --recursive https://github.com/ongy/waymonad
 * cd waymonad
 * `[PKG_CONFIG_PATH=/usr/local/lib/pkgconfig] cabal new-build`
 
 ### For unprivileged install:
 * Configure wlroots with: `meson build --prefix=<your prefix>`
 * ninja -C build install
 * `PKG_CONFIG_PATH=<your prefix>/lib/pkgconfig cabal new-build`
 
 ---
### For the little documentation there is for now: 
 * cabal new-haddock

-----

### What this is NOT

* A straight upgrade path
* A reimplementation of xmonad
* A full implementation containing DRM and other backends

### What this is

* Implemented in Haskell
* predictable layouting
* based on the compositor library [wlroots](https://github.com/SirCmpwn/wlroots)


