-- | 'Element', the type that pairs a component's size request with how to
-- measure and how to run it, and the protocol containers use to resolve a
-- content-dependent 'Blink.Layout.Constraints.Length' into a concrete
-- number. Also the generic attribute mechanism ('Attribute'\/'resolve')
-- used throughout Blink's own combinators to configure them, and the
-- layout attributes ('width'\/'height'\/'align') built on it.
module Blink.Element
  ( Element (..)
  , runElement
  , noIntrinsicSize
  , spacer
  , emptyElement
  , elementWithLayout
    -- * Attributes
  , Attribute (..)
  , resolve
    -- * Layout attributes
  , HasLayoutConfig (..)
  , width
  , height
  , align
  ) where

import Data.List (foldl')

import Blink.Geometry (Alignment (TopLeft), Orientation (..), Rectangle (..), Size (..))
import Blink.Layout.Constraints
  (Available (..), Layout (..), Length, MeasureCtx (..), exactly, fill, layoutWithConstraints, resolveLength)
import Blink.View (View, getBounds)

-- | The layout-facing pairing of a component's size request, its measure,
-- and its frame action. A container consumes a list of these to arrange a
-- set of children; a plain 'View' action has no way to declare either of the
-- first two, which is exactly the gap this type exists to fill.
data Element e msg = Element
  { elLayout  :: Layout
    -- ^ This element's size request: how it wants its extent determined on
    -- each axis, and how it sits within whatever slot it ends up with. Read
    -- by the parent, which has final say.
  , elMeasure :: MeasureCtx -> View e msg Size
    -- ^ This element's preferred size, given what the parent can offer.
    -- Called by the parent only when 'elLayout' names a content-dependent
    -- rule. Must not draw, emit, or mutate context.
  , elRun     :: View e msg ()
    -- ^ Runs the element for this frame within the current bounds, whatever
    -- they are: hit testing, focus, event dispatch and drawing alike. Must
    -- not read 'elLayout' or call 'elMeasure'.
  }

-- | Applies an element's own size request within the current bounds and
-- runs it. Used at the root of a view, and by a composite laying out
-- children it has already built. Resolves any content-dependent request
-- ('Blink.Layout.Constraints.fitContent', 'Blink.Layout.Constraints.atLeast',
-- 'Blink.Layout.Constraints.between') against the current bounds first --
-- unlike a box, which already knows the exact slot it is placing a child
-- into, the root has nothing narrower to offer than its own bounds on
-- either axis.
runElement :: Element e msg -> View e msg ()
runElement el = do
  r <- getBounds
  let avail = \o -> case o of
        Horizontal -> Bounded (rectWidth r)
        Vertical   -> Bounded (rectHeight r)
  w <- resolveLength Horizontal (layoutWidth  (elLayout el)) (avail Horizontal) (avail Vertical)   (elMeasure el)
  h <- resolveLength Vertical   (layoutHeight (elLayout el)) (avail Vertical)   (avail Horizontal) (elMeasure el)
  layoutWithConstraints (elLayout el) { layoutWidth = w, layoutHeight = h } (elRun el)

-- | For elements with no intrinsic size: whatever the parent can spare
-- along the axis being measured, or zero when the parent is itself sizing
-- to content on that axis.
noIntrinsicSize :: MeasureCtx -> View e msg Size
noIntrinsicSize ctx = pure $ case measureAxis ctx of
  Horizontal -> Size (availableOr 0 (measureMain ctx)) (availableOr 0 (measureCross ctx))
  Vertical   -> Size (availableOr 0 (measureCross ctx)) (availableOr 0 (measureMain ctx))
  where
    availableOr _ (Bounded v) = v
    availableOr d Unbounded   = d

-- | An element that draws nothing and takes whatever share it is given.
spacer :: Element e msg
spacer = Element (Layout fill fill TopLeft) noIntrinsicSize (pure ())

-- | An element that draws nothing and takes no space: @0x0@ on both axes,
-- unlike 'spacer', which fills whatever it's given. For the branch of a
-- conditional that has no content this frame but still needs to produce
-- an 'Element' — e.g. a container's 'Blink.Layout.Box.children' list
-- built with a plain @if@/@else@ rather than a list comprehension that can
-- just omit the entry.
emptyElement :: Element e msg
emptyElement = Element (Layout (exactly 0) (exactly 0) TopLeft) noIntrinsicSize (pure ())

-- | Pairs a plain 'View' action with an explicit size request, for use as a
-- container child before it reports its own 'Layout' (see "Blink.Controls").
-- Reports 'noIntrinsicSize', so @layout@ must not name a content-dependent
-- rule ('Blink.Layout.Constraints.fitContent', 'Blink.Layout.Constraints.atLeast',
-- or 'Blink.Layout.Constraints.between') -- there is nothing behind it to
-- measure.
elementWithLayout :: Layout -> View e msg () -> Element e msg
elementWithLayout layout ui = Element layout noIntrinsicSize ui

-- | A single field update on @cfg@, applied by 'resolve'.
newtype Attribute cfg = Attribute { runAttribute :: cfg -> cfg }

-- | Folds a list of attributes over a starting config, left to right -- a
-- later attribute setting the same field overrides an earlier one.
resolve :: cfg -> [Attribute cfg] -> cfg
resolve = foldl' (\cfg (Attribute f) -> f cfg)

-- | Implemented by any config type that nests a 'Layout', letting 'width'\/
-- 'height'\/'align' be applied to it directly -- the same delegation
-- pattern as 'Blink.Controls.Control.HasControlConfig', minus the
-- @e@\/@msg@ functional dependency that needs and this doesn't: a 'Layout'
-- is pure geometry, with no element-identity or message type of its own to
-- fix.
class HasLayoutConfig cfg where
  overLayout :: Attribute Layout -> Attribute cfg

instance HasLayoutConfig Layout where
  overLayout = id

-- | Sets the size request's width. See 'Length' for the available
-- constraints, and each control's own haddock for its default.
width :: HasLayoutConfig cfg => Length -> Attribute cfg
width l = overLayout (Attribute (\lay -> lay { layoutWidth = l }))

-- | Sets the size request's height. See 'Length' for the available
-- constraints, and each control's own haddock for its default.
height :: HasLayoutConfig cfg => Length -> Attribute cfg
height l = overLayout (Attribute (\lay -> lay { layoutHeight = l }))

-- | Sets how the control is positioned within its slot when it does not
-- fill the slot on one or both axes. Defaults to 'TopLeft'.
align :: HasLayoutConfig cfg => Alignment -> Attribute cfg
align a = overLayout (Attribute (\lay -> lay { layoutAlignment = a }))
