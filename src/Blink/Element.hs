{-# LANGUAGE FunctionalDependencies #-}
-- | 'Element', the type that pairs a component's size request with how to
-- measure and how to run it, and the protocol containers use to resolve a
-- content-dependent 'Blink.Layout.Constraints.Length' into a concrete
-- number. Also the generic attribute mechanism ('Attribute'\/'resolve')
-- used throughout Blink's own combinators to configure them, and the
-- layout attributes ('width'\/'height'\/'align') built on it.
module Blink.Element
  ( Element (..)
  , runElement
  , measureElement
  , noIntrinsicSize
  , spacer
  , emptyElement
  , elementWithLayout
    -- * Attributes
  , Attribute (..)
  , resolve
  , nested
  , appendTo
    -- * Layout attributes
  , HasLayoutConfig (..)
  , width
  , height
  , align
    -- * Attributes shared across widgets
    -- | Each is one attribute name that several widgets accept, each
    -- widget with its own type for it -- a slider's 'value' is a 'Double',
    -- a text input's is 'Data.Text.Text'.
  , HasValue (..)
  , HasStep (..)
  , HasOrientation (..)
  , HasItems (..)
  , HasItemAttrs (..)
  , HasSelection (..)
  , HasSelectionChanged (..)
  , HasContent (..)
  ) where

import Data.List (foldl')

import Blink.Geometry (Alignment (TopLeft), Orientation (..), Rectangle (..), Size (..))
import Blink.Layout.Constraints
  ( Available (..), Layout (..), Length, MeasureCtx (..)
  , exactly, fill, layoutWithConstraints, preferredSize, resolveLength
  )
import Blink.View (Effect, View, getBounds)

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
  (w, h) <- resolveElementLengths r el
  layoutWithConstraints (elLayout el) { layoutWidth = w, layoutHeight = h } (elRun el)

-- | The element's resolved size were it given @available@ to lay out
-- within -- the same resolve 'runElement' performs before calling
-- 'layoutWithConstraints', stopped short of actually running the element.
-- For a caller that needs to know an element's size before it has decided
-- where to place it -- see 'Blink.Popup.popup'.
measureElement :: Rectangle -> Element e msg -> View e msg Size
measureElement available el = do
  (w, h) <- resolveElementLengths available el
  pure $ Size (preferredSize w (rectWidth available)) (preferredSize h (rectHeight available))

-- | Resolves both axes' 'Length' against @available@ -- the two
-- 'resolveLength' calls 'runElement' and 'measureElement' both need before
-- they can do anything further with an element's size.
resolveElementLengths :: Rectangle -> Element e msg -> View e msg (Length, Length)
resolveElementLengths available el = do
  let avail = \o -> case o of
        Horizontal -> Bounded (rectWidth available)
        Vertical   -> Bounded (rectHeight available)
  w <- resolveLength Horizontal (layoutWidth  (elLayout el)) (avail Horizontal) (avail Vertical)   (elMeasure el)
  h <- resolveLength Vertical   (layoutHeight (elLayout el)) (avail Vertical)   (avail Horizontal) (elMeasure el)
  pure (w, h)

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

-- | Applies an attribute for a nested config to the config around it,
-- given how to read the nested field and how to replace it. The usual
-- body of an instance letting an attribute reach a nested config, e.g.
-- @overControl = nested bcControl (\\bc cc -> bc { bcControl = cc })@.
nested :: (outer -> inner) -> (outer -> inner -> outer) -> Attribute inner -> Attribute outer
nested get set (Attribute f) = Attribute (\o -> set o (f (get o)))

-- | An attribute that adds @h@ to the end of a list field, given how to
-- read the field and how to replace it -- the usual body of an @onX@
-- attribute, so each reaction added runs after the ones before it.
appendTo :: (cfg -> [h]) -> (cfg -> [h] -> cfg) -> h -> Attribute cfg
appendTo get set h = Attribute (\c -> set c (get c ++ [h]))

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

-- * Attributes shared across widgets

-- | A widget whose current value the caller sets, of type @v@.
class HasValue v cfg | cfg -> v where
  -- | The widget's current value -- caller-owned, passed back in every
  -- frame.
  value :: v -> Attribute cfg

-- | A widget that moves its value by a fixed amount per key or button
-- press.
class HasStep cfg where
  -- | How far one key or button press moves the value.
  step :: Double -> Attribute cfg

-- | A widget laid out along one axis.
class HasOrientation cfg where
  -- | Which axis the widget runs along.
  orientation :: Orientation -> Attribute cfg

-- | A widget built from a list of items of type @a@.
class HasItems a cfg | cfg -> a where
  -- | The data to build one item from, in order. A later 'items' replaces
  -- an earlier one rather than adding to it.
  items :: [a] -> Attribute cfg

-- | A widget whose items are each configured from their own data, by a
-- function of type @f@.
class HasItemAttrs f cfg | cfg -> f where
  -- | Attributes for the widget built from each item, given its data.
  itemAttrs :: f -> Attribute cfg

-- | A widget whose selection, of type @s@, the caller owns.
class HasSelection s cfg | cfg -> s where
  -- | The current selection -- caller-owned, passed back in every frame.
  selection :: s -> Attribute cfg

-- | A widget that reports a user-driven change to its selection.
class HasSelectionChanged e msg s cfg | cfg -> e msg s where
  -- | Reacts with the new selection whenever the user changes it. Store it
  -- and pass it back via 'selection'.
  onSelectionChanged :: (s -> [Effect e msg]) -> Attribute cfg

-- | A widget that shows a single child element.
class HasContent e msg cfg | cfg -> e msg where
  -- | The child element shown.
  content :: Element e msg -> Attribute cfg
