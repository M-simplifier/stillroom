module ComposeSpaces where

import Stillroom.Graphics.Space

invalid :: Transform World View
invalid = compose (identity :: Transform View View) (identity :: Transform World World)
