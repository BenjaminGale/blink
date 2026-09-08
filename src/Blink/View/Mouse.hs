{- |
Module: Blink.View.Mouse

Mouse position, button state, capture, geometric hover, and hit-rect
occlusion — the interaction queries 'Blink.View.Controls.control' uses to
implement the geometric hover model. See "Blink.View" for the module
overview; import that instead of this module directly.
-}
module Blink.View.Mouse
  ( getMousePos
  , isRegionHit
  , acquireCapture
  , registerMouseOver
  , wasMouseOverLastFrame
  , isAnyMouseOver
  , registerHitRect
  , isOccludedFor
  , isButtonDown
  , contextButtonDown
  , isButtonReleased
  , contextButtonReleased
  , isDragging
  , isMouseFree
  , getCaptured
  , contextCaptured
  , getMouse
  , contextMouse
  ) where

import qualified Data.Map.Strict as Map
import Blink.Geometry (Point, containsPoint)
import Blink.Input
  ( MouseCapture (..), ButtonState (..), captureOf
  , HoverState (..), wasHit, nextHoverState
  , Mouse (..), HitRect (..), inputMousePosition
  )
import Blink.View.Context

-- | The current frame's 'Mouse' state: button\/capture and per-element
-- hover.
getMouse :: View e msg (Mouse e)
getMouse = gets ctxMouse

-- | The current frame's 'Mouse' state, read directly from a 'ViewContext'
-- outside the 'View' monad.
contextMouse :: ViewContext e msg -> Mouse e
contextMouse = ctxMouse

-- | Modifies the current frame's 'Mouse'.
modifyMouse :: (Mouse e -> Mouse e) -> View e msg ()
modifyMouse f = modify $ \ctx -> ctx { ctxMouse = f (ctxMouse ctx) }

-- | The current mouse cursor position in window coordinates.
getMousePos :: View e msg Point
getMousePos = inputMousePosition <$> getInput

-- | 'True' when the mouse cursor is within the current bounds and within the
-- active interaction clip region (set by 'Blink.View.Drawing.withClip').
-- This is the lower-level, element-agnostic primitive; for a specific
-- control's hit area (bounds inset by its margin), see
-- 'Blink.View.Controls.isMouseOver'.
isRegionHit :: View e msg Bool
isRegionHit = do
  r    <- getBounds
  p    <- getMousePos
  clip <- getInteractionClip
  return $ containsPoint p r && maybe True (containsPoint p) clip

-- | Acquires mouse capture for the element if the left button is currently
-- down and nothing is captured yet, making this the first point of capture
-- for that press — the control a drag holds onto once the cursor leaves the
-- element that started it. Checked against both 'ButtonDown' and
-- 'ButtonHeld' since a press that started over one element (or over nothing)
-- can still be claimed by a different element the cursor moves onto later in
-- the same held press, as long as nothing else has claimed it first.
acquireCapture :: e -> View e msg ()
acquireCapture eid = modifyMouse $ \m -> m { mouseButton = case mouseButton m of
  ButtonDown MouseNotCaptured -> ButtonDown (MouseCapturedBy eid)
  ButtonHeld MouseNotCaptured -> ButtonHeld (MouseCapturedBy eid)
  btn                         -> btn
  }

-- | Records that the element was hit by the mouse this frame — typically
-- called after a geometric hit test such as 'isRegionHit' succeeds. Building
-- block for mouse-enter\/-exit: compare against 'wasMouseOverLastFrame' for
-- the same element to detect the transition. Any number of elements can each
-- call this in the same frame; all are remembered.
registerMouseOver :: Ord e => e -> View e msg ()
registerMouseOver eid = modifyMouse $ \m ->
  let prev = Map.findWithDefault NotOver eid (mouseHoverPrev m)
  in m { mouseHoverNext = Map.insert eid (nextHoverState prev True) (mouseHoverNext m) }

-- | 'True' when 'registerMouseOver' was called for the element on the
-- previous frame. Compare against this frame's own hit test to derive
-- mouse-enter (@not wasOver && isOver@) and mouse-exit (@wasOver && not
-- isOver@).
wasMouseOverLastFrame :: Ord e => e -> View e msg Bool
wasMouseOverLastFrame eid = gets $ \ctx ->
  wasHit (Map.findWithDefault NotOver eid (mouseHoverPrev (ctxMouse ctx)))

-- | 'True' when 'registerMouseOver' has been called for any element so far
-- this frame. Reflects the whole frame's hover set only once every control
-- that might register one has run — call it after the rest of the view, the
-- same way the hover getter under the legacy single-owner hover model this
-- replaces was read at the end of a frame, for controls built with
-- geometric hover (many elements can be "over" at once, so unlike that
-- legacy getter there is no single element to name — only whether the set
-- is non-empty). Every entry in 'mouseHoverNext' was written by
-- 'registerMouseOver', which only ever records a hit, so a non-empty map
-- here is exactly "some element was hit this frame".
isAnyMouseOver :: View e msg Bool
isAnyMouseOver = gets (not . Map.null . mouseHoverNext . ctxMouse)

-- | Records this frame's current bounds as the element's hit-tested rect,
-- tagged with a registration index one past whatever's already been
-- registered this frame -- so visiting order (a control before whatever it
-- goes on to render) is recoverable later via 'isOccludedFor'. Call only
-- after a geometric hit test such as 'isRegionHit' succeeds (and the
-- element is otherwise eligible, e.g. not disabled) -- an element nowhere
-- near the pointer never needs an entry, which keeps this map's size
-- proportional to whatever's actually under the pointer, not the size of
-- the whole view.
registerHitRect :: Ord e => e -> View e msg ()
registerHitRect eid = do
  r <- getBounds
  modifyMouse $ \m ->
    let idx = Map.size (mouseHitRectsNext m)
    in m { mouseHitRectsNext = Map.insert eid (HitRect r idx) (mouseHitRectsNext m) }

-- | 'True' when, per last frame's 'registerHitRect' calls, some other
-- element was hit at the current mouse position with a higher registration
-- index than this one -- i.e. something nested inside this element, or
-- drawn after it, sat on top of it there. An element not registered last
-- frame (just appeared, or wasn't hit) is never considered occluded --
-- occlusion is judged against last frame's picture, so a control that is
-- itself brand new at this spot fails open for one frame, the same
-- trade-off overlapping-widget resolution in other immediate-mode
-- GUIs (Dear ImGui, egui) accepts.
--
-- Meant to gate a control's own 'acquireCapture' (see
-- 'Blink.View.Controls.Control.watchHover'): a container backs off letting
-- a nested child it rendered after it — and which is consequently ahead of
-- it in next frame's registration order — win capture for a click that
-- landed on the child, instead of the container claiming it purely because
-- its own hit test ran first.
isOccludedFor :: Ord e => e -> View e msg Bool
isOccludedFor eid = do
  p    <- getMousePos
  prev <- gets (mouseHitRectsPrev . ctxMouse)
  case Map.lookup eid prev of
    Nothing               -> pure False
    Just (HitRect _ myIdx) -> pure $ any (occludes myIdx p) (Map.toList (Map.delete eid prev))
  where
    occludes myIdx p (_, HitRect r idx) = idx > myIdx && containsPoint p r

-- | 'True' when the left button is currently held, whether this is the
-- first frame of the press or a later one -- callers that only care whether
-- the button is currently down, not which frame of the press this is, don't
-- need to distinguish 'ButtonDown' from 'ButtonHeld'.
isButtonDown :: View e msg Bool
isButtonDown = gets contextButtonDown

-- | 'True' when the left button is currently held, read directly from a
-- 'ViewContext' outside the 'View' monad.
contextButtonDown :: ViewContext e msg -> Bool
contextButtonDown ctx = case mouseButton (ctxMouse ctx) of
  ButtonDown _ -> True
  ButtonHeld _ -> True
  _            -> False

-- | 'True' on the one frame the left button transitions from held to up.
isButtonReleased :: View e msg Bool
isButtonReleased = gets contextButtonReleased

-- | 'True' on the one frame the left button transitions from held to up,
-- read directly from a 'ViewContext' outside the 'View' monad.
contextButtonReleased :: ViewContext e msg -> Bool
contextButtonReleased ctx = case mouseButton (ctxMouse ctx) of
  ButtonReleased _ -> True
  _                -> False

-- | 'True' on every frame that the given element is being dragged — from the
-- initial press through to release.
isDragging :: Eq e => e -> View e msg Bool
isDragging eid = (== MouseCapturedBy eid) <$> gets contextCaptured

-- | Which element currently holds mouse capture, if any. Exported for
-- control authors that need to inspect capture state directly, e.g. when
-- implementing focus-on-click without using 'Blink.View.Controls.control'.
getCaptured :: View e msg (MouseCapture e)
getCaptured = gets contextCaptured

-- | Which element currently holds mouse capture, if any, read directly from
-- a 'ViewContext' outside the 'View' monad.
contextCaptured :: ViewContext e msg -> MouseCapture e
contextCaptured = captureOf . mouseButton . ctxMouse

-- | 'True' when no element currently holds mouse capture — i.e. no drag is
-- in progress. Use alongside 'isDragging' to decide whether a control should
-- respond to hover: @free || dragging@ allows hover when the mouse is
-- uncontested or when this element itself owns the capture.
isMouseFree :: View e msg Bool
isMouseFree = isNotCaptured <$> gets contextCaptured
  where
    isNotCaptured MouseNotCaptured = True
    isNotCaptured (MouseCapturedBy _) = False
