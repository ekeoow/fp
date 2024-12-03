aux :: [Int]
aux = 1 : 2 : aux

selfrle :: [Int]
selfrle = 1 : 2 : x
  where
    x = tail $ tail (concat (zipWith replicate selfrle aux))
