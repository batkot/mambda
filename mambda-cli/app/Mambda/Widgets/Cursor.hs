module Mambda.Widgets.Cursor (
    cursorFrame,
) where

import Brick qualified
import Data.Text qualified as Text
import Prelude

cursorFrame :: Integer -> Brick.Widget n
cursorFrame tick = Brick.str . Text.unpack $ cursorAnimSprites Prelude.!! (fromInteger tick `mod` Prelude.length cursorAnimSprites)

cursorAnimSprites :: [Text.Text]
cursorAnimSprites = ["⠇", "⡆", "⣄", "⣠", "⢰", "⠸", "⠙", "⠋"]
