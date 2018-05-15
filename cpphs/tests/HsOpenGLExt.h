/* -----------------------------------------------------------------------------
 *
 * Module      :  GL extension support for Graphics.Rendering.OpenGL
 * Copyright   :  (c) Sven Panne 2002-2004
 * License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
 * 
 * Maintainer  :  sven.panne@aedion.de
 * Stability   :  provisional
 * Portability :  portable
 *
 * This header should only define preprocessor macros!
 *
 * -------------------------------------------------------------------------- */

#ifndef HSOPENGLEXT_H
#define HSOPENGLEXT_H

/* NOTE: The macro must immediately start with the foreign declaration,
   otherwise the magic mangler (hack_foreign) in the Hugs build system
   doesn't recognize it. */
#define EXTENSION_ENTRY(_msg,_entry,_ty) \
foreign import CALLCONV unsafe "dynamic" dyn_/**/_entry :: Graphics.Rendering.OpenGL.GL.Extensions.Invoker (_ty) ; \
_entry :: (_ty) ; \
_entry = dyn_##_entry ptr_##_entry ; \
ptr_/**/_entry :: FunPtr a ; \
ptr_/**/_entry = unsafePerformIO (Graphics.Rendering.OpenGL.GL.Extensions.getProcAddress (_msg) ("_entry")) ; \
{-# NOINLINE ptr_/**/_entry #-}

#endif

EXTENSION_ENTRY("GL_EXT_fog_coord or OpenGL 1.4",glFogCoorddEXT,GLdouble -> IO ())
