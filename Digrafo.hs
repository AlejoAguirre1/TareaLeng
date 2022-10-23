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
arcos (G xs y) = concat [ crearTupla n (y n) | n <- xs ]

-- (nVertices G) es el número de vértices de G
nVertices :: Digrafo v -> Int
nVertices (G x _) = length x

-- (nArcos G) es el número de arcos de G
nArcos :: Digrafo v -> Int
nArcos (G xs y) = length (concat [ crearTupla n (y n) | n <- xs ])

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
depthFirstSearch (G xs y) n = depthSearch [] [n] y

-- (topologicalSort G) es la lista que corresponde con el ordenamiento topológico de G
topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort (G xs y) = topoSort xs (concat [ crearTupla n (y n) | n <- xs ])


-- ............................................................................................... --


-- toma un vertice y sus sucesores y lo comvierte a una lista de tuplas de la forma (vertice , arcoN)
crearTupla :: v -> [v] -> [(v,v)]
crearTupla n arco = do 
            let len = length arco           -- consiguiendo la cantidad de arcos
            let vertice = replicate len n   -- replicando en una lista el vertice [vertice,vertice,....,vertice * longitud arcos]
            zip vertice arco                -- creando la lista de arcos


-- genera una lista de vertices partiendo de un vertice y realizando una buqueda en profundidad
depthSearch :: Eq v => [v] -> [v] -> (v -> [v]) -> [v]
depthSearch solucion verticesSucesores y = if null verticesSucesores            -- se conprueba si la lista de vertices sucesores esta vacia
                                                    then solucion               -- si se encuentra vacia de se devuelve la lista solucion
                                                    else if (head verticesSucesores) `elem` solucion            -- si no esta vacia se comprueba si el vertice ya esta e la lista solucion
                                                        then depthSearch solucion (tail verticesSucesores) y    -- si se encuentra se quita de la lista de vertices sucesores y se llama a depthSearch
                                                        else depthSearch (solucion ++ take 1 verticesSucesores) ((y (head verticesSucesores)) ++ tail verticesSucesores ) y   -- si no esta se agrega a la lista solucion y se agragan sus sucesores a la lista de vertices sucesores y se llama a depthSearch

-- genera una lista con vertices ordenados topologicamente
topoSort :: Eq v => [v] -> [(v,v)] -> [v]
topoSort vertice arco = do
                    if null vertice -- se comprueba si existen vertices
                        then []     -- si no existen vertices se retorna una lista vacia
                        else do 
                            let solucion = conseguirSolucion vertice arco --se obtinen los primeros vertices que no tienen antecesores
                            solucion ++ conseguirSolucion (vertice \\ solucion) (eliminarArcos solucion arco)    -- se llama recusivamente eliminando los vertices sin antesesores y sus arcos respectivamente


-- regresa una lista con los vertices que no tienen antecesores
conseguirSolucion :: Eq v => [v] -> [(v,v)] -> [v]
conseguirSolucion vertice arco = do
                        if null vertice     -- se verifica que existan vertices
                            then vertice    -- si no existen vertices se retorna una lista vacia
                            else if null [ y | x <- arco , y <- take 1 vertice , y == snd x ]   -- se verifica si el primer vertice tiene antesesores
                                    then vertice                                                -- si no tiene antesesores se retorna la lista
                                    else conseguirSolucion (tail vertice) arco                   -- si tiene antesesores se quita de la lista y se llama recursivamente

-- elimna las tuplas de arcos de una lista de vertices                        
eliminarArcos :: Eq v => [v] -> [(v,v)] -> [(v,v)]
eliminarArcos vertice arco = [ x | x <- arco , y <- take 1 vertice, y /= fst x ] -- busca en cada elemnto de una lista de vertices y elimna las tuplas que contengan sus arcos