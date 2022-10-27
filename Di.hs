module GrafoConVectorDeAdyacencia
	(Orientacion (..),
		Grafo,
		creaGrafo, -- (Ix v,Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p	
		dirigido, -- (Ix v,Num p) => (Grafo v p) -> Bool
		adyacentes, -- (Ix v,Num p) => (Grafo v p) -> v -> [v]
		nodos, -- (Ix v,Num p) => (Grafo v p) -> [v]
		aristas, -- (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
		aristaEn, -- (Ix v,Num p) => (Grafo v p) -> (v,v) -> Bool
		peso -- (Ix v,Num p) => v -> v -> (Grafo v p) -> p
	) where


import Data.Array

data Orientacion = D | ND
					deriving (Eq, Show)

data Grafo v p = G Orientacion (Array v [(v,p)])
					deriving (Eq, Show)

creaGrafo :: (Ix v, Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p

creaGrafo D cs vs =
	G ND (accumArray
		(\xs x -> xs ++ [x]) [] cs
		[(x1,(x2,p)) | (x1,x2,p) <- vs])

creaGrafo ND cs vs =
	G D (accumArray
		(\xs x -> xs ++ [x]) [] cs
		([(x2,(x1,p)) | (x1,x2,p) <- vs, x1 /= x2] ++
		[(x1,(x2,p)) | (x1,x2,p) <- vs]))

dirigido :: (Ix v,Num p) => (Grafo v p) -> Bool
dirigido (G o _) = o == D

dirigido :: (Ix v,Num p) => (Grafo v p) -> Bool
dirigido (G o _) = o == D

adyacentes :: (Ix v,Num p) => (Grafo v p) -> v -> [v]
adyacentes (G _ g) v = map fst (g!v)

nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
nodos (G _ g) = indices g

nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
nodos (G _ g) = indices g

peso :: (Ix v,Num p) => v -> v -> (Grafo v p) -> p
peso x y (G _ g) = head [c | (a,c) <- g!x , a == y]

aristaEn :: (Ix v,Num p) => (Grafo v p) -> (v,v) -> Bool
aristaEn g (x,y) = y `elem` adyacentes g x

aristas :: (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
aristas (G o g) = [(v1,v2,w) | v1 <- nodos (G o g) , (v2,w) <- g!v1]





grafo G [1,2,3] suc where suc 1 =[1,3]; suc 2 = [1,3]; suc 3 = [1,2]; suc _ = []





{-
                    ---------
                    v       |
                --->1------>2---
                |   |       ^   |
                |   |       |   |
                |   v       |   |
                ----3--------   |
                    ^           |
                    |           |
                    -------------



sucesor :: Int -> [Int]
sucesor 1 = [2,3]; sucesor 2 = [1,3]; sucesor 3 = [1,2]; sucesor _ = []

grafo = G [1,2,3] sucesor


grafo = G [1,2,3] sucesor where sucesor 1 = [2,3]; sucesor 2 = [1,3]; sucesor 3 = [1,2]; sucesor _ = []


---------------------------------------------------------------------------------------------------------


                            4------->2<-----5
                            |
                            |
                            |
                            v
                            1------->3
grafo2 = G [1,2,3,4,5] sic where sic 1 = [3]; sic 2 = []; sic 3 = []; sic 4 = [1,2]; sic 5 = [2]
sucesor :: Int -> [Int]

grafo = G [1,2,3,4,5] sucesor

grafo = G [1,2,3,4,5] sucesor where sucesor 1 = [3]; sucesor 2 = []; sucesor 3 = []; sucesor 4 = [1,2]; sucesor 5 = [2]


grafo = G [1,2,3,4,5] sucesor where sucesor 1 = [3]; sucesor 2 = []; sucesor 3 = []; sucesor 4 = [1,2];sucesor 5 = [2]; sucesor _ = []

-}
--grafo1 = G [1..4] suc where suc 1 = [2,3]; suc 2 = [4]; suc 3 = [4]; suc _ = []
