module Highlighter
  (
  ) where

import Data.Text (Text)
import Text.Regex.TDFA

data Highlighter = Highlighter [(Text, Text)]

createHighlighter :: [(Text, Text)] -> Highlighter
createHighlighter = Highlighter

highlight :: Highlighter -> Text -> Text
highlight (Highlighter ms) text = undefined
