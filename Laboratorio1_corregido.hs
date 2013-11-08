module Code where
import Prelude (Show)

data Bool = False | True
	deriving (Show)

not :: Bool -> Bool
not = \x -> case x of {
		False -> True;
		True -> False
}

(||) :: Bool -> Bool -> Bool
(||) = \x -> \y-> case x of {
		False -> y;
		True -> True
}

data N = O | S N
	deriving (Show)

par :: N -> Bool
par = \n -> case n of {
		O -> True;
		S x -> not(par x)
}

times :: (a -> a) -> N -> (a -> a)
times = \f -> \n -> case n of {
		O -> \x -> x;
		S y -> \x -> f (times f y x)
}

--times (+(S O)) (S(S(O))) (S(S(S(O))))

(+) :: N -> N -> N
(+) = times S


mod2 :: N -> N
mod2 = \n -> case par n of {
						False -> S O ;
						True -> O
						}

mkeven :: N -> N
mkeven = \n -> case (par n) of {
					False -> S n ;
					True -> n
					}

(<) :: N -> N -> Bool
(<) = \m -> \n  -> case m of {
	O -> case n of {
		O -> False;
		S y -> True
	};
	S x -> case n of {
		O -> False;
		S y -> x < y
	}
}

-- Devuleve True si algún número menor o igual a n satisface p
anyleq :: N -> (N -> Bool) -> Bool
anyleq = \n -> \f -> case n of {
		O -> f O;
		S y -> (anyleq y f) || f (S y)
}

-- Devuelve el mayor número positivo menor a n que satisface p.
-- De no existir, devuelve O
maxpos :: N -> (N -> Bool) -> N
maxpos = \n -> \f -> case n of {
		O -> O;
		S y -> case (f y) of {
				True -> y ;
				False -> (maxpos y f)
		}
}