module Sandbox where

{-
  A functor is a map from one category to another: 
  it maps objects into objects and morphisms into morphisms. 
  </summary>
  <remarks>
  http://en.wikipedia.org/wiki/Functor
  In category theory a functor is a special type of mapping between categories.
  Functors can be thought of as homomorphisms between categories, or 
  morphisms in the category of  small categories.
  
  Let C and D be categories. A functor F from C to D is a mapping that:
  
  • associates to each object x ϵ C an object F(X) ϵ D,
  
  • associates to each morphism f: X → Y ϵ C a morphism F(f): F(Y) → F(X) ϵ D
    such that the following two conditions hold:
    
  • F(idₓ) = idF(ₓ) for every object x ϵ C 
    
  • F(g ◦ f) = F(f) ◦ F(f) for all morphisms f: X → Y and g: Y → Z
    
  That is, functors must preserve identity morphisms and composition of
  morphisms.
  
  
           F(f)
  F(A) -------------> F(B)
   ^        ^         ^
   |        |         |
   |     Lifting      |
   |        |         | 
   |        f         |
   A  --------------> B
  
  
  
  Notation
  ϵ  is element of
  Ʉ  for all
  э  there exists
  ˸  such that
-}

-- Un elemento de la categoría A
a :: Int
a = 2

-- Un morfismo f: A → B
f :: Int -> Double
f x = (fromIntegral x) * 2.5

-- Un elemento de la categoría B
-- terminal del morfismo f(a)
b :: Double
b = f a

-- El elemento a mapeado a la categoría F(A)
a_ :: [Int]
a_ = [ a ]

lift_ f = map f

-- El morfismo f mapeado al morfismo F(f)
f_ = lift_ f

-- El elemento b mapeado a la categoría F(B)
b_ = f_ a_

main = do print ("a: " ++ show a)
          print ("b: " ++ show b)
          print ("a_: " ++ show a_)
          print ("b_: " ++ show b_)

