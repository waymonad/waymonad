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

---

### Things I did by accident that are now a feature

I was working on `view`/`greedyView` implementation and accidently mapped the same workspace to two outputs.
That looked weird, so I implemented a logic that will give the layout a box centered the respective outputs with width and height being the minimum from all outputs the workspace is shown on.

Now it's a feature!
![Workspace on two outputs](https://i.imgur.com/FD53fWa.jpg)
