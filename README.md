# waymonad

We all love tiling window managers and most of all we love [xmonad](https://github.com/xmonad/xmonad).
As you may know, [wayland](https://wayland.freedesktop.org/) is trying to replace X11.

This brings problems for us, since xmonad will not work with the new architecture.

This project is intended to provide a wayland based desktop which shares the ideals and experience from xmonad.

## [WIP]: This is work in Progress.
### There's no bar/quality of life

If you need to lock your session: swaylock should work perfectly fine.
Currently that's not set up in any automatic way though.

Swaybg should work, and my waymonad-clients has a somewhat usable background application.

### "Minor" things

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


# The fabulous logo:

<img src="./assets/logo-heavy.svg">
<img src="./assets/logo-light.svg">
