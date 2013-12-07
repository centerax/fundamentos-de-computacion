module Laboratorio3 where
--autores
-- Pablo Benitez
-- Matias Settimo

--Programas
data Prog = Ass [(Var,Expr)] | Seq Prog Prog | If [(Guarda,Prog)] | Do [(Guarda,Prog)]
  deriving Show

--Expresiones
type Var = Char

data Expr = Num Int | Var Var | Op Oper Expr Expr | Minus Expr
  deriving Show

data Oper = Add | Sub | Mul | Div | Mod
  deriving Show

--Guardas
data Guarda = Comp COp Expr Expr |Not Guarda| BOp Con Guarda Guarda | T | F
   deriving Show

data COp = Eq | Dif | Less | LEq | Gr | GEq
   deriving Show

data Con =  And | Or
   deriving Show

--Ejecución de un programa
run :: Prog -> Memoria -> Memoria
run = \p m -> case p of {
			Ass ves -> case allValues (evalExprs m ves) of {
						False -> error "variable indefinida" ;
						True -> updates m (evalExprs m ves) };
			Seq p1 p2 -> run p2 (run p1 m);
			If rs -> case choice m rs of   {
						Nothing -> error "seleccion vacia" ;
						Just p1 -> run p1 m};
			Do rs -> case choice m rs of {
						Nothing -> m;
						Just p1 -> run (Seq p1 p) m}}

choice :: Memoria -> [(Guarda,Prog)] -> Maybe Prog
choice = \m -> \h -> case h of {
	[] -> Nothing;
	x:xs -> case x of {
		(i, j) -> case (evalGuarda m i) of {
			Nothing -> choice m xs;
			Just z -> case z of {
				True -> Just j;
				False -> choice m xs
			}
		}
	}
}

--Memoria
type Estado = Maybe Int
--type Memoria =  Var -> Estado
type Memoria = [(Var,Estado)]
--Tambien se puede elegir la siguiente definicion: type Memoria =  [(Var , Estado )]

--Funciones a implementar:
evalExpr :: Memoria -> Expr -> Estado
evalExpr = \m -> \h -> case h of {
	Num n ->  Just n;
	Var x -> case lookp m x of {
		Nothing -> Nothing;
		Just z -> Just z
	};
	Op w x y -> case evalExpr m x of {
		Nothing -> Nothing;
		Just a -> case evalExpr m y of {
			Nothing -> Nothing;
			Just b -> case w of {
				Add -> Just (a + b);
				Sub -> Just (a - b);
				Mul -> Just (a * b);
				Div -> Just (div a b);
				Mod -> Just (mod a b)
			}
		}
	};
	Minus x -> case evalExpr m x of {
		Nothing -> Nothing;
		Just z -> Just (-1 * z)
	}
}

lookp :: Memoria -> Var -> Estado
lookp = \m -> \h -> case m of {
	[] -> Nothing;
	x:xs -> case x of {
		(i,j) -> case (i == h) of {
		True -> j;
		False -> lookp xs h
				}
			}
		}

evalExprs :: Memoria -> [(Var,Expr)] -> [(Var,Estado)]
evalExprs = \m -> \h -> case h of {
	[] -> [];
	x:xs -> case x of {
		(i, j) -> (i, evalExpr m j):(evalExprs m xs)
	}
}

allValues :: [(Var,Estado)] -> Bool
allValues = \h -> case h of {
	[] -> True;
	x:xs -> case x of {
		(i, j) -> case j of {
			Nothing -> False;
			Just z -> True && (allValues xs)
		}
	}
}

updates :: Memoria -> [(Var,Estado)] -> Memoria
updates = \m -> \h -> case h of {
		[] -> m;
		x:xs -> (updates (update m x) xs)
		}

update :: Memoria -> (Var,Estado) -> Memoria
update = \m -> \h -> case h of {
				(i,j) -> case (lookp m i) of {
							Nothing -> h:m;
							Just e -> case m of {
								x:xs -> case x of {
								(a,b) -> case (a == i) of {
									True -> (a,j):xs;
									False -> x:(update xs h)
											}
										}
									}
							}
						}



evalGuarda :: Memoria -> Guarda -> Maybe Bool
evalGuarda = \m -> \h -> case h of {
	Comp z x y -> case (evalExpr m x) of {
					Nothing -> Nothing;
					Just a -> case (evalExpr m y) of {
								Nothing -> Nothing;
								Just b -> case z of {
											Eq -> Just (a == b);
											Dif -> Just (a /= b);
											Less -> Just (a < b);
											LEq -> Just (a <= b);
											Gr -> Just (a > b);
											GEq -> Just (a >= b)
											};
								}
					};
	Not x -> case (evalGuarda m x) of {
				Nothing -> Nothing;
				Just z -> Just (not z);
				};
	BOp z x y -> case (evalGuarda m x) of {
					Nothing -> Nothing;
					Just a -> case (evalGuarda m y) of {
								Nothing -> Nothing;
								Just b -> case z of {
											And -> Just (a && b);
											Or -> Just (a || b)
											};

								}
				};
	T -> Just True;
	F -> Just False
}

inicial :: Memoria
inicial = []

--Ejemplos de programas para probar
--Variables
x::Var
x= 'x'
y::Var
y= 'y'
z::Var
z= 'z'

--Programas
swap :: Prog
swap = Seq (Ass [(x,Num 1),(y,Num 2)])
		   (Ass [(x,Var y),(y,Var x)])
--lookp (run swap inicial) x => Just 2
--lookp (run swap inicial) y => Just 1

minimo :: Int -> Int-> Prog
-- guarda en z el mínimo de m y n
minimo = \ m n -> Seq  (Ass [(x,Num m),(y,Num n)])
					(If [(Comp Less (Var x) (Var y),Ass[(z,Var x)]),
					(Comp Eq (Var x) (Var y),Ass[(z,Var x)]),
					(Comp Less (Var y) (Var x),Ass[(z,Var y)])])
--lookp (run (min 4 7) inicial) z  => Just 4

mcd :: Int -> Int-> Prog
--Algoritmo de Euclides, guarda en z el mcd de m y n
mcd = \m n -> Seq
			  (Seq (Ass [(x,Num m),(y,Num n)])
				   (Do [(Comp Less (Var x) (Var y),Ass[(y,Op Sub(Var y)(Var x))]),
					   (Comp Less (Var y) (Var x),Ass[(x,Op Sub(Var x)(Var y))])]))
				   (Ass [(z,Var x)])
--lookp (run (mcd 1005 450) inicial) z => Just 15
--lookp (run (mcd 48 30) inicial) z => Just 6