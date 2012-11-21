module Sandbox where

{-
  Monad (category theory)
  http://en.wikipedia.org/wiki/Monad_(category_theory)
  In category theory, a monad or triple is an (endo-)functor, together with two 
  associated natural  transformations. 
  
  If F and G are a pair of adjoint functors, with F left adjoint to G, then the 
  composition F ◦ G  is a monad.
  
  Therefore, a monad is an endofunctor.
  
  If F and G are inverse functors the corresponding monad is the identity 
  functor.
  
  If C is a category, a monad on C consists of:
  
    • a functor T: C → C
    
    • together with two natural transformations:
      
      • η: 1C  → T  where 1C denotes the identity functor on C
  
      • μ: T²  → T  where T² is the functor T ◦ T from C to C
  
  These are required to fulfill the following axioms:
  
    • μ ◦ Tμ =  μ ◦ μT  
      as natural transformations T³ → T
      This axiom is akin to the associativity in monoids. 
  
    • μ ◦ Tη =  μ ◦ ηT = 1T  where 1T denotes the identity transformation 
      from T to T as natural transformations T → T
      This second axiom is akin to the existence of an identity element.
  
   A monad on C can alternatively be defined as a monoid in the category EndC
   whose objects are the endofunctors of C and whose morphisms are the natural
   transformations between them, with the monoidal structure induced by the 
   composition of endofunctors.
   
  We can rewrite these conditions using following commutative diagrams:
  
  
                     T(μ x)
       T(T(T(X))) -------------> T(T(X))
         |                         |
         |                         |
  μ T(x) |                         | μ x
         |                         | 
         v                         v
         T(T(X))  --------------> T(X)
                       μ x
  
  
                     ηT(X)
       T(X) -------------------> T(T(X))
         |  \                      |
         |       \                 |
  T(ηx)  |            \            | μ x
         |                 \       | 
         v                     \   v
         T(T(X))  --------------> T(X)
                      μ x
  
  Notation
  η eta
  μ my

-}

{-
  Monad
  http://en.wikipedia.org/wiki/Monad_(functional_programming)
  In functional programming, a monad is a kind of abstract data type constructor
  used  to represent computations (instead of data in the domain model).
  
  Monads allow the programmer to chain actions together to build a pipeline,
  in which each action is decorated with additional processing rules provided by 
  the monad.
  
  Programs written in functional style can make use of monads to structure 
  procedures that include sequenced operations,[1][2] or to define some 
  arbitrary control flows (like handling concurrency, 
  continuations, side effects such as input/output, or exceptions).
  
  The usual formulation of a monad for programming is known as a Kleisli 
  triple, and has the  following components:
 
  A type construction that defines, for every underlying type, how to obtain a 
  corresponding monadic type.
  
  In Haskell's notation, the name of the monad represents the type constructor.
  If M is the name of the monad and t is a data type, then "M t" is the 
  corresponding type in the monad.
 
  A MUnit function that maps a value in an underlying type to a value in the 
  corresponding monadic type.
  
  The result is the "simplest" value in the corresponding type that completely 
  preserves the original value (simplicity being understood appropriately to 
  the monad). In Haskell, this function is called return
  due to the way it is used in the do-notation described later.
  
  The MUnit function has the polymorphic type t→M t.
 
  A binding operation of polymorphic type (M t)→(t→M u)→(M u), which Haskell 
  represents by the infix operator >>=. Its first argument is a value in a 
  monadic type, its second argument is a  function that maps from the underlying 
  type of the first argument to another monadic type, and its
  result is in that other monadic type.
  
  The binding operation can be understood as having four stages:
  
  The monad-related structure on the first argument is "pierced" to expose 
  any number of values in the underlying type t.
  
  The given function is applied to all of those values to obtain 
  values of type (M u).
  
  The monad-related structure on those values is also pierced, 
  exposing values of type u.
  
  Finally, the monad-related structure is reassembled over all of the 
  results, giving a single value of type (M u).
 
 
  Axioms
  For a monad to behave correctly, the definitions must obey a few axioms.
  [8] (The ≡ symbol is not Haskell code, but indicates an equivalence between
  two Haskell expressions.)
  "return" acts approximately as a neutral element of >>=.
  (return x) >>= f ≡ f x
  m >>= return ≡ m
 
  Binding two functions in succession is the same as binding one 
  function that can be determined from them.
  (m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )
  In the last rule, the notation \x -> defines an anonymous function that maps 
  any value x to the expression that follows.
  In mathematical notation, the axioms are:
  
  (return x) >>= f  == f x
 
  m >>= return == m
 
  (m >>= f) >>= g == m >>= (\x . (f x >>= g) 

-}

{-
  Monads for the Curious Programmer
  http://bartoszmilewski.wordpress.com/2011/01/09/monads-for-the-curious-programmer-part-1/
  
  A monad is an endofunctor together with two special families of morphisms, 
  both going vertically, one up and one down:

    The one going up is called Unit(η).

    The one going down is called Join (μ).

  Unit takes a value from the poorer type, then picks one value from the 
  richer type, and pronounces the two roughly equivalent. 
  Such a rough equivalent of True from the Bool object is the 
  one-element list [True] from the [Bool] object.
  Similarly, MUnit would map False into [False].
  It would also map integer 5 into [5] and so on.
  
  Unit can be though of as immersing values from a lower level into the higher
  level in the most natural way possible.
  By the way, in programming we call a family of functions defined for any type 
  a polymorphic function.
  
  To explain Join, imagine the functor acting twice.
  For instance, from a given type T the list functor will first 
  construct the type [T] (list of T), and then [[T]] (list of list of T).
  Join removes one layer of “listiness” by joining the sub-lists.
  Plainly speaking, it just concatenates the inner lists.
  Given, for instance, [[a, b], [c], [d, e]], it produces [a, b, c, d, e].
  It’s a many-to-one mapping from the richer type to the poorer type.

-}

{- 
 Unit
 
 Unit can be though of as immersing values from a lower level into the
 higher level  in the most natural way possible.

 return :: (Monad m) => a -> m a

-}
monadUnit :: a -> [a]
monadUnit x = [x]


{- 
  Join

  To explain Join, imagine the functor acting twice.
  For instance, from a given type T the list functor will first 
  construct the type [T] (list of T), and then [[T]] (list of list of T).
  Join removes one layer of “listiness” by joining the sub-lists.
  
  join :: (Monad m) => m (m a) -> m a
-}
monadJoin :: [[a]] -> [a]
monadJoin x = foldr (++) [] x

{-
  Bind

 (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

 join :: Monad m => m (m a) -> m a

 Equivalence between bind and join

 join x = x >>= id

 x >>= f = join (fmap f x)

-}
monadBind :: [a] -> (a -> [b]) -> [b]
monadBind m k = foldr ((++) . k) [] m

-- Un elemento T(X)
t1 :: [Int]
t1 = [2, 3]

-- El elemento T(x) mapeado a T(T(X))
t2 :: [[Int]]
t2 = monadUnit t1

-- El elemento T(T(X)) mapeado a T(X)
t3 :: [Int]
t3 = monadJoin t2

-- El elemento T(T(X)) mapeado a T(X)
t3_ :: [Int]
t3_ = monadBind t2 (\x -> x)

{-
           F(f)
  F(A) -------------> F(B)
   ^        ^         ^
   |        |         |
   |     Lifting      |
   |        |         | 
   |        f         |
   A  --------------> B
-}

-- Un elemento de la categoría A
a :: Int
a = 2

-- Un morfismo f: A → B
f :: Int -> Double
f x = ((fromIntegral ((x)::Int))::Double) * 2.5

-- Un elemento de la categoría B
-- terminal del morfismo f(a)
b :: Double
b = f a

-- El elemento a mapeado a la categoría F(A)
a2 :: [Int]
a2 = [a]

{-
  Ahora necesitamos mapear 
  
   b Double → b2 [Double]

   haríamos 
  
    b2 = f a2 
  
  pero f admite Int, no admite [Int]
   
  es necesario transformar f para que admita [Int]
  
  f2 = lift f

  b2 = f2 a2 
  
-}

liftFunc :: (a -> b) -> [a] -> [b]
liftFunc f = map f

-- El morfismo f mapeado al morfismo F(f)
f2 :: [Int] -> [Double]
f2 = liftFunc f

-- El elemento b mapeado a la categoría F(B)
b2 = f2 a2


-- El elemento a mapeado a la categoría F(F(A))
a3 :: [[Int]]
a3 = [[a]]

{-
  Ahora necesitamos mapear 
  
   b Double → b3 [[Double]]

   haríamos 
  
    b3 = f2 a3 
  
  pero f2 admite [Int], no admite [[Int]]
   
  es necesario transformar f2 para que admita [[Int]]
  
  f3 = lift f2

  b3 = f3 a3 
  
-}

-- El morfismo f2 mapeado al morfismo F(F(f))
f3 :: [[Int]] -> [[Double]]
f3 = liftFunc f2

-- El elemento b mapeado a la categoría F(F(B))
b3 = f3 a3

{-
  Tenemos la función 
    f :: Int -> Double
  
  Ahora necesitamos aplicarla a [Int] 

  Para aplicar f sobre [Int] podemos:
   
   'bajar' los elementos de [Int] a Int
   operar sobre el elemento Int
   finalmente 'subir' el resultado a [Double]
  
  bind nos permite 'bajar' la categoría
  de cada elemento contenido en la colección.
  
  unit nos permite 'subir' la categoría
  de un elemento.
-}
items2 :: [Int]
items2 = [1,2,3,4,5,6,7]

res2 = monadBind items2 (\x -> monadUnit (f x))

{- 
  Usando la clase Monad
  
  bind equivale a >>=
  
  unit equivale a return
-}
res2a = items2 >>= (\x -> return (f x))

{- El operador $ nos permite quitar paréntesis -}
res2c = items2 >>= (\x -> return $ f x)

{- 
  Usando la sintaxis do {}
-}
res2d = do {x <- items2; return $ f x}

{-
  Tenemos la función 
    f :: Int -> Double
  
  Ahora necesitamos aplicarla a [[Int]]

  Para aplicar f sobre [Int] podemos:
   
   'bajar' los elementos de [[Int]] a [Int] y después a [Int]
   operar sobre el elemento Int
   finalmente 'subir' el resultado a [Double] y después a [[Double]]
  
  bind nos permite 'bajar' la categoría
  de cada elemento contenido en la colección.
  
  unit nos permite 'subir' la categoría
  de un elemento.
-}
items3 :: [[Int]]
items3 = [[1],[2,3,4],[5,6,7]]

res3 = monadBind items3 (\x -> monadUnit(monadBind x (\y -> monadUnit (f y))))

{- 
  Usando la clase Monad 
  
  bind equivale a >>=
  
  unit equivale a return
  
  El operador $ nos permite quitar paréntesis
-}
res3a = items3 >>= (\x -> return $ x >>= (\y -> return $ f y))

{- 
  Usando la sintaxis do {}
-}
res3b = do{x <- items3; y <- x; return $ f y}

{- 
-}
vals2 :: [Int]
vals2 = [1,2,3,4,5,6,7]

coef2 :: [Int]
coef2 = [1,2,3,4,5,6,7]



