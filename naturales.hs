module FC where
import Prelude (Show)

data N = O | S N
	deriving Show

--s :: N -> N
--s = \x -> S x

suma :: N -> N -> N
(suma) = \m -> \n -> case m of {O -> n ; S x -> S ( suma x n )}

--sum2 :: N -> N
--sum2 = \x -> S(S(x))

max :: N -> N -> N
max = \m -> \n -> case m of {
	O -> n;
	S x -> case n of {
	O -> m;
	S y -> S (min x y)
}
}

min :: N -> N -> N
min = \m -> \n -> case m of {
	O -> O;
	S x -> case n of {
	O -> O;
	S y -> S (min x y)
}
}

doble :: N -> N
doble = \n -> case n of {
	O -> O;
	S x -> S (S (doble (x)))
}

sumleq :: N -> N
sumleq = \n -> case n of {
	O -> O;
	S x -> (suma (S(sumleq(x))) x)
}