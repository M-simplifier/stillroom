module Stillroom.Landscape.Grass.Kernel
  ( BladeInputs (..), CornerInputs (..), BladeAttributes (..),
    bladeCorner, grassShapeGLSL ) where

data BladeInputs a = BladeInputs
  { root :: !(a,a,a), height :: !a, width :: !a, side :: !(a,a), curve :: !a }

data CornerInputs a = CornerInputs
  { t :: !a, profile :: !a, sideSign :: !a, uvV :: !a }

data BladeAttributes a = BladeAttributes
  { position :: !(a,a,a), normal :: !(a,a,a), texCoord :: !(a,a), extra :: !(a,a) }

-- The authored arithmetic is shared by Float expansion and GLSL generation.
-- Trigonometry stays on the CPU, including the four template row profiles.
bladeCorner :: Fractional a => BladeInputs a -> CornerInputs a -> BladeAttributes a
bladeCorner blade corner =
  let (x,y,z) = root blade
      (sideX,sideZ) = side blade
      rowT = t corner
      w = width blade*(1-rowT)*profile corner
      px = x+curve blade*rowT*rowT
      py = y+height blade*rowT
      pz = z+(-curve blade*0.28*rowT*rowT)
      offset = sideSign corner*w
   in BladeAttributes
      { position = (px+sideX*offset,py+0*offset,pz+sideZ*offset)
      , normal = (-sideZ,0.18,sideX)
      , texCoord = (rowT,uvV corner)
      , extra = (x,z)
      }
{-# INLINE bladeCorner #-}
{-# SPECIALIZE bladeCorner :: BladeInputs Float -> CornerInputs Float -> BladeAttributes Float #-}

data Expr
  = Input String | Scalar Float | Neg Expr | Add Expr Expr | Sub Expr Expr
  | Mul Expr Expr | Divide Expr Expr | Absolute Expr | Sign Expr
  | JourneyUV Expr Expr

instance Num Expr where
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate = Neg
  abs = Absolute
  signum = Sign
  fromInteger = Scalar . fromInteger

instance Fractional Expr where
  (/) = Divide
  fromRational = Scalar . fromRational

render :: Expr -> String
render expression = case expression of
  Input name -> name
  Scalar value -> show value
  Neg a -> "(-"++render a++")"
  Add a b -> binary "+" a b
  Sub a b -> binary "-" a b
  Mul a b -> binary "*" a b
  Divide a b -> binary "/" a b
  Absolute a -> "abs("++render a++")"
  Sign a -> "sign("++render a++")"
  JourneyUV a b -> "(journey?"++render a++":"++render b++")"
  where binary operator a b = "("++render a++operator++render b++")"

grassShapeGLSL :: String
grassShapeGLSL =
  let x = Input "rootHeight.x"; y = Input "rootHeight.y"; z = Input "rootHeight.z"
      inputs = BladeInputs
        { root = (x,y,z), height = Input "rootHeight.w", width = Input "shape.x"
        , side = (Input "shape.y",Input "shape.z"), curve = Input "shape.w" }
      corner = CornerInputs
        { t = Input "corner.x", profile = Input "corner.y", sideSign = Input "corner.z"
        , uvV = JourneyUV y (Input "corner.z") }
      attributes = bladeCorner inputs corner
      vec3 (a,b,c) = "vec3("++render a++","++render b++","++render c++")"
      vec2 (a,b) = "vec2("++render a++","++render b++")"
   in unlines
      [ "// Generated from Stillroom.Landscape.Grass.Kernel.bladeCorner."
      , "void grassShape(vec3 corner,vec4 rootHeight,vec4 shape,bool journey,"
      , "    out vec3 position,out vec3 normal,out vec2 texCoord,out vec2 extra) {"
      , "    position="++vec3 (position attributes)++";"
      , "    normal="++vec3 (normal attributes)++";"
      , "    texCoord="++vec2 (texCoord attributes)++";"
      , "    extra="++vec2 (extra attributes)++";"
      , "}"
      ]
