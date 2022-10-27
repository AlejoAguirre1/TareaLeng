{- 
--     Tarea #3: Lenguajes de Programación
--     José Aguirre
--     V-27.129.140 

--     Javier Castrillo
--     V-27. 
-}

module Digrafo (Digrafo(G),
                vertices,
                arcos,
                nVertices,
                nArcos,
                sucesores,
                antecesores,
                gradoSal,
                gradoEnt,
                depthFirstSearch,
                topologicalSort) where

import Data.List ( (\\) )

data Digrafo v = G [v] (v -> [v])

-- (vertices G) es la lista de los vértices de G
vertices :: Digrafo v -> [v]
vertices (G x _) = x

-- (arcos G) es la lista de los arcos de G
arcos :: Digrafo v -> [(v, v)]
arcos (G xs y) = concat [ crearTuplas n (y n) | n <- xs ]

-- (nVertices G) es el número de vértices de G
nVertices :: Digrafo v -> Int
nVertices (G x _) = length x

-- (nArcos G) es el número de arcos de G
nArcos :: Digrafo v -> Int
nArcos (G xs y) = length (concat [ crearTuplas n (y n) | n <- xs ])

-- (sucesores G x) es la lista de sucesores directos del vértice x en G
sucesores :: Eq v => Digrafo v -> v -> [v]
sucesores (G _ y) n = y n

-- (antecesores G x) es la lista de antecesores directos del vértice x en G
antecesores :: Eq v => Digrafo v -> v -> [v]
antecesores (G xs y) n = [ x | x <- xs, n `elem` y x ]

-- (gradoSal G x) es el número de sucesores directos del vértice x en G
gradoSal :: Eq v => Digrafo v -> v -> Int
gradoSal (G _ y) n = length (y n)
    
-- (gradoEnt G x) es el número de antecesores directos del vértice x en G
gradoEnt :: Eq v => Digrafo v -> v -> Int
gradoEnt (G xs y) n = length [ x | x <- xs, n `elem` y x ]

-- (depthFirstSearch G x) es la lista de todos los vértices alcanzables desde x en G, aplicando búsqueda en profundidad
depthFirstSearch :: Eq v => Digrafo v -> v -> [v]
depthFirstSearch (G xs y) n = busquedaAux [] [n] y

-- (topologicalSort G) es la lista que corresponde con el ordenamiento topológico de G
topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort (G xs y) = sortAux xs (concat [ crearTuplas n (y n) | n <- xs ])


-- ............................................................................................... --
-- ............................................................................................... --

--                                   Definición de funciones auxiliares

-- ............................................................................................... --
-- ............................................................................................... --


-- toma un vertice y sus sucesores y lo comvierte a una lista de tuplas de la forma (vertice , arcoN)
crearTuplas :: v -> [v] -> [(v,v)]
crearTuplas n listArcos = do 
            let k = length listArcos        -- cuenta los arcos para saber la longitud de la lista
            let listVerts = replicate k n   -- replica k veces el vertice n en una lista length([n, n,..., n]) = k
            zip listVerts listArcos         -- crea una lista de tuplas [(n,a1),(n,a2),...,(n,ak-1),(n, ak)]


-- genera una lista de vertices partiendo de un vertice y realizando una buqueda en profundidad
busquedaAux :: Eq v => [v] -> [v] -> (v -> [v]) -> [v]
busquedaAux sol verticesSucesores y = if null verticesSucesores            -- se conprueba si la lista de vertices sucesores esta vacia
                                                    then sol               -- si se encuentra vacia de se devuelve la lista sol
                                                    else if (head verticesSucesores) `elem` sol            -- si no esta vacia se comprueba si el vertice ya esta e la lista sol
                                                        then busquedaAux sol (tail verticesSucesores) y    -- si se encuentra se quita de la lista de vertices sucesores y se llama a busquedaAux
                                                        else busquedaAux (sol ++ take 1 verticesSucesores) ((y (head verticesSucesores)) ++ tail verticesSucesores ) y   -- si no esta se agrega a la lista sol y se agragan sus sucesores a la lista de vertices sucesores y se llama a busquedaAux

-- genera una lista con vertices ordenados topologicamente
sortAux :: Eq v => [v] -> [(v,v)] -> [v]
sortAux vertice arco = do
                    if null vertice -- se comprueba si existen vertices
                        then []     -- si no existen vertices se retorna una lista vacia
                        else do 
                            let sol = hallarSol vertice arco --se obtinen los primeros vertices que no tienen antecesores
                            sol ++ hallarSol (vertice \\ sol) (quitarArcos sol arco)    -- se llama recusivamente eliminando los vertices sin antesesores y sus arcos respectivamente


-- regresa una lista con los vertices que no tienen antecesores
hallarSol :: Eq v => [v] -> [(v,v)] -> [v]
hallarSol vertice arco = do
                        if null vertice     -- se verifica que existan vertices
                            then vertice    -- si no existen vertices se retorna una lista vacia
                            else if null [ y | x <- arco , y <- take 1 vertice , y == snd x ]   -- se verifica si el primer vertice tiene antesesores
                                    then vertice                                                -- si no tiene antesesores se retorna la lista
                                    else hallarSol (tail vertice) arco                   -- si tiene antesesores se quita de la lista y se llama recursivamente

-- elimna las tuplas de arcos de una lista de vertices                        
quitarArcos :: Eq v => [v] -> [(v,v)] -> [(v,v)]
quitarArcos vertice arco = [ x | x <- arco , y <- take 1 vertice, y /= fst x ] -- busca en cada elemnto de una lista de vertices y elimna las tuplas que contengan sus arcos
