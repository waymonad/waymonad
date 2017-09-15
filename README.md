# waymonad

We all love tiling window managers and most of all we love [xmonad](https://github.com/xmonad/xmonad).
As you may know, [wayland](https://wayland.freedesktop.org/) is trying to replace X11.

This brings problems for us, since xmonad will not work with the new architecture.

This project is intended to provide a wayland based desktop which shares the ideals and experience from xmonad.

## [WIP]: This is work in Progress.
Currently more a place for me to write down thoughts and provide a platform to discuss them.

### What this is NOT

* A straight upgrade path
* A reimplementation of xmonad
* A full implementation containing DRM and other backends

### What this is

* Implemented in Haskell
* predictable layouting
* based on the compositor library [wlroots](https://github.com/SirCmpwn/wlroots)

