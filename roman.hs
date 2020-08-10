solution :: Integer -> String
solution n = map ((!!)rs) $ concat $ go n $ zip [0..] zs

go 0 _ = []
go n ((i,d):(j,f):ss) | n-d>=0 = d:(go (n-d) zs) | n-f>0 = it i n | otherwise = go n (j,f):ss
go n [d] = [d]

it i n = 
  where d = ds!!i

ds = [1000,500,100,50,10,5,1]
zs=zip [0..] ds
rs= "MDCLXVI"
