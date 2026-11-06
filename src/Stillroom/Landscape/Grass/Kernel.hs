module Stillroom.Landscape.Grass.Kernel
  ( bladeCorner, grassShapeGLSL ) where

-- The authored arithmetic is shared by Float expansion and GLSL generation.
-- Trigonometry stays on the CPU, including the four template row profiles.
bladeCorner
  :: Fractional a
  => (a,a,a) -> a -> a -> (a,a) -> a -> (a,a,a) -> a
  -> ((a,a,a),(a,a,a),(a,a),(a,a))
bladeCorner (x,y,z) height width (sideX,sideZ) curve (t,profile,sideSign) uvV =
  let w = width*(1-t)*profile
      px = x+curve*t*t
      py = y+height*t
      pz = z+(-curve*0.28*t*t)
      offset = sideSign*w
   in ((px+sideX*offset,py+0*offset,pz+sideZ*offset),
       (-sideZ,0.18,sideX),(t,uvV),(x,z))
{-# INLINE bladeCorner #-}
{-# SPECIALIZE bladeCorner :: (Float,Float,Float) -> Float -> Float -> (Float,Float) -> Float -> (Float,Float,Float) -> Float -> ((Float,Float,Float),(Float,Float,Float),(Float,Float),(Float,Float)) #-}

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
      t = Input "corner.x"; profile = Input "corner.y"; sideSign = Input "corner.z"
      (position,normal,texCoord,extra) = bladeCorner (x,y,z) (Input "rootHeight.w")
        (Input "shape.x") (Input "shape.y",Input "shape.z") (Input "shape.w")
        (t,profile,sideSign) (JourneyUV y sideSign)
      vec3 (a,b,c) = "vec3("++render a++","++render b++","++render c++")"
      vec2 (a,b) = "vec2("++render a++","++render b++")"
   in unlines
      [ "// Generated from Stillroom.Landscape.Grass.Kernel.bladeCorner."
      , "void grassShape(vec3 corner,vec4 rootHeight,vec4 shape,bool journey,"
      , "    out vec3 position,out vec3 normal,out vec2 texCoord,out vec2 extra) {"
      , "    position="++vec3 position++";"
      , "    normal="++vec3 normal++";"
      , "    texCoord="++vec2 texCoord++";"
      , "    extra="++vec2 extra++";"
      , "}"
      ]
