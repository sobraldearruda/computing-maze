module Matrix where
-- Módulo que lida com operações sobre matrizes

-- gera uma matriz mxn
generateMatrix :: Int -> Int -> a -> [[a]]
generateMatrix m n a = replicate m (replicate n a)

-- Função para obter o número de linhas
numRows :: [[a]] -> Int
numRows = length

-- Função para obter o número de colunas
numCols :: [[a]] -> Int
numCols mat = case mat of
    [] -> 0  -- Matriz vazia, zero colunas
    (row:_) -> length row  -- Número de colunas é o comprimento da primeira linha

-- Modifica o valor na posição (i, j) da matriz
updateMatrix :: [[a]] -> Int -> Int -> a -> [[a]]
updateMatrix matrix i j newValue = 
    take i matrix ++                                  -- Pega as linhas antes da linha i
    [updateRow (matrix !! i) j newValue] ++           -- Modifica a linha i
    drop (i + 1) matrix                               -- Pega as linhas depois da linha i
  where
    updateRow row j newValue = 
        take j row ++ [newValue] ++ drop (j + 1) row  -- Modifica o elemento na coluna j
