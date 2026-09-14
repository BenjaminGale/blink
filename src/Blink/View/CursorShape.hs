{- |
Module: Blink.View.CursorShape

The mouse pointer shape a control requests for the current frame, via
'requestCursor'. See "Blink.View" for the module overview; import that
instead of this module directly.
-}
module Blink.View.CursorShape
  ( requestCursor
  ) where

import Blink.Rendering (CursorShape)
import Blink.View.Context

-- | Requests that the backend display the given pointer shape for the
-- remainder of this frame, overriding whatever's been requested so far.
-- Typically called from a control's hover or drag check (e.g. alongside
-- 'Blink.View.isDragging' or 'Blink.View.wasMouseOverLastFrame') --
-- there's no need to also request 'Blink.Rendering.CursorArrow' when the
-- condition doesn't hold, since every frame already starts there.
--
-- Last call wins: since controls are visited in draw order, whatever's
-- drawn on top of the pointer requests last, matching what's visually
-- under it. 'Blink.View.getCursorShape' reads back the frame's final
-- value.
requestCursor :: CursorShape -> View e msg ()
requestCursor shape = modifyOut $ \out -> out { outCursorShape = shape }
