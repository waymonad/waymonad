# waymonad

We all love tiling window managers and most of all we love [xmonad](https://github.com/xmonad/xmonad).
As you may know, [wayland](https://wayland.freedesktop.org/) is trying to replace X11.

This brings problems for us, since xmonad will not work with the new architecture.

This project is intended to provide a wayland based desktop which shares the ideals and experience from xmonad.

## [WIP]: This is work in Progress.
### You may test this, but currently it's not nice to your battery!
The [damage tracking PR](https://github.com/swaywm/wlroots/pull/571) currently being worked on for wlroots should fix this.
Currently waymonad will render your entire desktop at the outputs framerate, even if nothing changed.

### There's no bar/background application/quality of life

Until `surface-layers` is in, these won't be implemented.
I have no idea when that happens, so until then expect a rather barebones experience (I may implement background).

### "Minor" things

* I don't have a floating layer yet. Though override redirect (dmenu) works.
* There is barely any documentation so unless you are familiar with Haskell (and preferably xmonad) this may be rather hard to get comfortable with.

-----
# "Install" (compile and execute locally):

To test this, you need `wlroots` installed.
This currently only builds with the `new-build` feature of cabal-install. `stack` is (not yet) supported.

 * git clone --recursive https://github.com/ongy/waymonad
 * cd waymonad
 * cabal new-build
 
 * cabal new-haddock
 ^ this will generate about the only documentation there is for now. Sorry!
-----

### What this is NOT

* A straight upgrade path
* A reimplementation of xmonad
* A full implementation containing DRM and other backends

### What this is

* Implemented in Haskell
* predictable layouting
* based on the compositor library [wlroots](https://github.com/SirCmpwn/wlroots)


