{- 
--     Tarea #3: Lenguajes de Programación
--     José Aguirre
--     V-27.129.140 

--     Javier Castrillo
--     V-27.372.931
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

import Data.List ( (\\) ) -- Para obtener elementos en comun entre dos listas

data Digrafo v = G [v] (v -> [v])

-- (vertices G) es la lista de los vértices de G
vertices :: Digrafo v -> [v]
vertices (G x _) = x  
-- Se obtiene solamente x (la cantidad de vértices)

-- (arcos G) es la lista de los arcos de G
arcos :: Digrafo v -> [(v, v)]
arcos (G xs y) = concat [ construirArcos n (y n) | n <- xs ] 
-- Crea una lista con tuplas representando los arcos

-- (nVertices G) es el número de vértices de G
nVertices :: Digrafo v -> Int
nVertices (G x _) = length x 
-- Obtiene la longitud de x, es decir, la cantidad de vertices

-- (nArcos G) es el número de arcos de G
nArcos :: Digrafo v -> Int
nArcos (G xs y) = length (concat [ construirArcos n (y n) | n <- xs ]) 
-- Se obtiene la longitud de la lista de arcos 

-- (sucesores G x) es la lista de sucesores directos del vértice x en G
sucesores :: Eq v => Digrafo v -> v -> [v]
sucesores (G _ y) n = y n 
-- Se obtienen los sucesores partiendo de un vértice n

-- (antecesores G x) es la lista de antecesores directos del vértice x en G
antecesores :: Eq v => Digrafo v -> v -> [v]
antecesores (G xs y) n = [ x | x <- xs, n `elem` y x ] 
-- Se obtienen los antecesores partiendo de un vértice n

-- (gradoSal G x) es el número de sucesores directos del vértice x en G
gradoSal :: Eq v => Digrafo v -> v -> Int
gradoSal (G _ y) n = length (y n)  
-- Obtiene la longitud de la lista de sucesores
    
-- (gradoEnt G x) es el número de antecesores directos del vértice x en G
gradoEnt :: Eq v => Digrafo v -> v -> Int
gradoEnt (G xs y) n = length [ x | x <- xs, n `elem` y x ] 
-- Obtiene la longitud de la lista de antecesores

-- (depthFirstSearch G x) es la lista de todos los vértices alcanzables desde x en G, aplicando búsqueda en profundidad
depthFirstSearch :: Eq v => Digrafo v -> v -> [v]
depthFirstSearch (G xs y) n = busquedaAux [] [n] y 
-- Realiza una búsqueda con una lista vacía llamando a una función auxiliar

-- (topologicalSort G) es la lista que corresponde con el ordenamiento topológico de G
topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort (G xs y) = ordenarAux xs (concat [ construirArcos n (y n) | n <- xs ]) 
-- A partir de una lista de arcos, se realiza el ordenamiento a través de una función auxiliar ordenarAux


-- ............................................................................................... --
-- ............................................................................................... --

--                                   Definición de funciones auxiliares

-- ............................................................................................... --
-- ............................................................................................... --


-- A partir de un vertice y sus sucesores crea lista de tuplas con cada vértice y arco
construirArcos :: v -> [v] -> [(v,v)]
construirArcos n listArcos = do 
            let k = length listArcos        -- Cuenta los arcos para saber la longitud de la lista
            let listVerts = replicate k n   -- Replica k veces el vertice n en una lista length([n, n,..., n]) = k
            zip listVerts listArcos         -- Crea una lista de tuplas [(n,a1),(n,a2),...,(n,ak-1),(n, ak)]


-- Función auxiliar que produce una lista de vertices a partir de un vertice
busquedaAux :: Eq v => [v] -> [v] -> (v -> [v]) -> [v]
busquedaAux sol listVertSuc y = if null listVertSuc    -- Comprueba si la lista de vertices sucesores es vacia
                                            then sol               -- Si es vacia, devuelve la lista solución
                                            else if (head listVertSuc) `elem` sol   -- Si no es vacia, comprueba si el vertice ya está en la lista solución
                                                then busquedaAux sol (tail listVertSuc) y  -- Si se encuentra, se quita de la lista de vertices sucesores y se llama a busquedaAux
                                                else busquedaAux (sol ++ take 1 listVertSuc) ((y (head listVertSuc)) ++ tail listVertSuc ) y   -- Si no está, se agrega a la lista sol y se agragan sus sucesores a la lista de vertices sucesores y se llama a busquedaAux

-- Función auxiliar que produce una lista con vertices ordenados topológicamente
ordenarAux :: Eq v => [v] -> [(v,v)] -> [v]
ordenarAux vertice arco = do
                    if null vertice -- Verifica si hay vertices
                        then []     -- Si no hay, retorna una lista vacia
                        else do 
                            let sol = hallarSol vertice arco -- Sino, se obtienen los primeros vertices sin antecesores
                            sol ++ hallarSol (vertice \\ sol) (quitarArcos sol arco)    -- se llama recusivamente eliminando los vertices sin antecesores y sus arcos respectivamente


-- Función auxiliar que regresa una lista con los vertices sin antecesores
hallarSol :: Eq v => [v] -> [(v,v)] -> [v]
hallarSol vertice arco = do
                if null vertice     --  Verifica si hay vertices
                    then vertice    --  Si no hay, se retorna una lista vacia
                    else if null [ y | x <- arco , y <- take 1 vertice , y == snd x ]   -- Verifica si el primer vertice tiene tiene antecesores
                            then vertice                                                -- Si no tiene, retorna la lista de vertices
                            else hallarSol (tail vertice) arco                   -- Si tiene, se llama recursivamente sin incluirlo (Tomando solo la cola de la lista)

-- Función auxiliar que elimina los arcos que van hacia un vértice en especígico                        
quitarArcos :: Eq v => [v] -> [(v,v)] -> [(v,v)]
quitarArcos vertice arco = [ x | x <- arco , y <- take 1 vertice, y /= fst x ] 
-- Busca un vértice en la lista de arcos y elimina todas las tuplas que lo contengan 