-- | 'Element', the type that pairs a component's size request with how to
-- measure and how to run it, and the protocol containers use to resolve a
-- content-dependent 'Length' into a concrete number.
module Blink.UI.Element
  ( Element (..)
  , runElement
  , noIntrinsicSize
  , spacer
  , elementWithLayout
  , withLayout
  , resolveLength
  ) where

import Blink.Geometry (Alignment (TopLeft), Orientation (..), Rectangle (..), Size (..))
import Blink.Layout.Constraints (Available (..), Layout (..), Length (..), MeasureCtx (..), layoutWithConstraints)
import Blink.UI (UI, getBounds)

-- | The layout-facing pairing of a component's size request, its measure,
-- and its frame action. A container consumes a list of these to arrange a
-- set of children; a plain 'UI' action has no way to declare either of the
-- first two, which is exactly the gap this type exists to fill.
data Element e msg = Element
  { elLayout  :: Layout
    -- ^ This element's size request: how it wants its extent determined on
    -- each axis, and how it sits within whatever slot it ends up with. Read
    -- by the parent, which has final say.
  , elMeasure :: MeasureCtx -> UI e msg Size
    -- ^ This element's preferred size, given what the parent can offer.
    -- Called by the parent only when 'elLayout' names a content-dependent
    -- rule. Must not draw, emit, or mutate context.
  , elRun     :: UI e msg ()
    -- ^ Runs the element for this frame within the current bounds, whatever
    -- they are: hit testing, focus, event dispatch and drawing alike. Must
    -- not read 'elLayout' or call 'elMeasure'.
  }

-- | Applies an element's own size request within the current bounds and
-- runs it. Used at the root of a view, and by a composite laying out
-- children it has already built. Resolves any content-dependent request
-- ('FitContent', 'AtLeast', 'Between') against the current bounds first --
-- unlike a box, which already knows the exact slot it is placing a child
-- into, the root has nothing narrower to offer than its own bounds on
-- either axis.
runElement :: Element e msg -> UI e msg ()
runElement el = do
  r <- getBounds
  let avail = \o -> case o of
        Horizontal -> Bounded (rectWidth r)
        Vertical   -> Bounded (rectHeight r)
  w <- resolveLength Horizontal layoutWidth  (avail Horizontal) (avail Vertical)   el
  h <- resolveLength Vertical   layoutHeight (avail Vertical)   (avail Horizontal) el
  layoutWithConstraints (elLayout el) { layoutWidth = w, layoutHeight = h } (elRun el)

-- | For elements with no intrinsic size: whatever the parent can spare
-- along the axis being measured, or zero when the parent is itself sizing
-- to content on that axis.
noIntrinsicSize :: MeasureCtx -> UI e msg Size
noIntrinsicSize ctx = pure $ case measureAxis ctx of
  Horizontal -> Size (availableOr 0 (measureMain ctx)) (availableOr 0 (measureCross ctx))
  Vertical   -> Size (availableOr 0 (measureCross ctx)) (availableOr 0 (measureMain ctx))
  where
    availableOr _ (Bounded v) = v
    availableOr d Unbounded   = d

-- | An element that draws nothing and takes whatever share it is given.
spacer :: Element e msg
spacer = Element (Layout Fill Fill TopLeft) noIntrinsicSize (pure ())

-- | Pairs a plain 'UI' action with an explicit size request, for use as a
-- container child before it reports its own 'Layout' (see "Blink.Controls").
-- Reports 'noIntrinsicSize', so @layout@ must not name a content-dependent
-- rule ('FitContent', 'AtLeast', or 'Between') -- there is nothing behind it
-- to measure.
elementWithLayout :: Layout -> UI e msg () -> Element e msg
elementWithLayout layout ui = Element layout noIntrinsicSize ui

-- | Overrides an already-built element's size request, e.g. to slot a
-- composite built elsewhere into a container at a specific width, height,
-- or alignment. Unlike 'elementWithLayout', this starts from an 'Element'
-- that already has its own layout and measure, and replaces only the former.
withLayout :: Layout -> Element e msg -> Element e msg
withLayout layout el = el { elLayout = layout }

-- | Turns a possibly content-dependent 'Length' into a pure one by
-- measuring the element when its request names 'FitContent', 'AtLeast', or
-- 'Between'. @mainOf@ extracts the constraint being resolved (the element's
-- width or height) from its 'Layout'; @orientation@ says which of those two
-- axes that is, so the element's 'elMeasure' can answer accordingly.
resolveLength
  :: Orientation
  -> (Layout -> Length)
  -> Available
  -> Available
  -> Element e msg
  -> UI e msg Length
resolveLength orientation mainOf mainAvail crossAvail el =
  case mainOf (elLayout el) of
    FitContent  -> Exactly              <$> preferred
    AtLeast l   -> Exactly . max l      <$> preferred
    Between l h -> Exactly . clampTo l h <$> preferred
    l           -> pure l
  where
    preferred = sizeAlong orientation <$> elMeasure el MeasureCtx
      { measureAxis  = orientation
      , measureMain  = mainAvail
      , measureCross = crossAvail
      }
    clampTo lo hi = max lo . min hi

sizeAlong :: Orientation -> Size -> Double
sizeAlong Horizontal = sizeWidth
sizeAlong Vertical   = sizeHeight
