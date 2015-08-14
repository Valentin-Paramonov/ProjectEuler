coins = [1,2,5,10,20,50,100,200]

ways _ 0 = 1
ways 1 _ = 1
ways c s = sum . map (\n -> ways (next c) (s - n*c)) $ [0..cw] where
    cw = s `div` c

next 1 = 1
next c = last . takeWhile (< c) $ coins

main = print . ways 200 $ 200
