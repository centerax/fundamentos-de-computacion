import qualified Prelude
import Prelude hiding ((+),(-), (/), (*), (^), (<), (==), (<=), (>=), (>),pred, mod, div, filter, map, (++), drop, take, dropWhile, takeWhile)


--data Bool = False | True
--	deriving (Show)

data N = O | S N
	deriving (Show)

(+)::N->N->N
(+)  = \n-> \m-> case n of {O-> m; (S x)-> S(x+m)}

(*)::N->N->N
(*)  = \n-> \m-> case n of {O-> O; (S x)-> m+ (x*m)}

(^)::N->N->N
(^)  = \n-> \m-> case m of {O-> S O; (S x)-> n*(n^x)}



pred::N->N
pred = \n -> case n of {O->O; (S x)->x}


(-)::N->N->N
(-)  = \n-> \m-> case m of {O-> n; (S x)-> ((pred n)-x)}

(==)::N->N->Bool
(==) = \n-> \m-> case n of {O-> case m of { O -> True; (S y)-> False}; (S x) -> case m of {O-> False; (S y)-> x == y}}

(>) ::N->N->Bool
(>) = \n-> \m-> case n of {O-> False ; (S x) -> case m of { O-> True; (S y)-> x>y}}

(<) ::N->N->Bool
(<) = \n-> \m-> m>n

(>=)::N->N->Bool
(>=) = \n-> \m-> (n==m)||(n>m)

(<=)::N->N->Bool
(<=) = \n-> \m-> (n==m)||(n<m)

div::N->N->N
div = \n-> \m-> case n>=m of {False-> O;  True-> S(div (n-m) m)}

mod::N->N->N 
mod = \n-> \m-> case n>=m of {False-> n;  True-> ( mod (n-m) m)}

pos::N->Bool
pos = \n-> case n of {O -> False; (S x)-> True}

largo :: [a]->N
largo = \h-> case h of {
						[] -> O;
						(x:xs)->S(largo xs)
						}
						
(++) :: [a] -> [a] -> [a]
(++) = \h -> \i -> case h of {
							[] -> i;
							(x:xs) -> x:(xs ++ i)
							}

filter :: (a-> Bool) -> [a] -> [a]
filter = \p -> \h -> case h of {
								[] -> [];
								(x:xs) -> case p x of {
														True -> x:(filter p xs);
														False -> (filter p xs)
														}
								}

map :: (a->b) -> [a] -> [b]
map = \f -> \h -> case h of {
							[] -> [];
							(x:xs) -> (f x):(map f xs)
							}
							
reverso :: [a] -> [a]
reverso = \h -> case h of {
							[] -> [];
							(x:xs) -> (reverse xs) ++ [x]
							}
							
splitAT :: N -> [a] -> ([a], [a])
splitAT = \n -> \h -> case n of {
									O -> ([],h);
									S x -> case h of {
													[] -> ([],[]);
													(z:zs) -> case (splitAT x zs) of {
																					(j,k) -> (z:j,k)
																					}
													}
								}
								
initi :: [a] -> [a]
initi = \h -> case h of { 
						[] -> [];
						(x:xs) -> case xs of {
											[] -> [];
											(z:zs) -> x : init xs
											}
						}

takeWhile :: (a->Bool) -> [a] -> [a]
takeWhile = \p -> \h -> case h of {
									[] -> [];
									(x:xs) -> case p x of {
															False -> xs;
															True -> x:(takeWhile p xs)
															}
									}

dropWhile :: (a->Bool) -> [a] -> [a]
dropWhile = \p -> \h -> case h of {
									[] -> [];
									(x:xs) -> case p x of {
															False -> x:xs;
															True -> (dropWhile p xs)
															}
									}
									
fromTO  :: N -> N -> [N]
fromTO = \n -> \m -> case (n > m) of { 
								True -> [];
								False -> n:(fromTO (S n) m)
								}

factores :: N -> [N]
factores = \n -> (filter (\x -> (mod n x) == O) (fromTO (S(O)) n)  )

										
							