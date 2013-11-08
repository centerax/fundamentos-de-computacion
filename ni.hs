module FC where
import Prelude(Show)

data Bool= False| True
	deriving Show

ni :: Bool -> Bool -> Bool
(ni) = \x -> \y -> case x of {True -> False;False -> case y of {True->False;False->True}}