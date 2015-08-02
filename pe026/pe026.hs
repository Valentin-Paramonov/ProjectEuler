fracCycle n d = fracCycle' [r] r d where
    r = n `rem` d
fracCycle' _ 0 _ = []
fracCycle' rs n d
    | r `elem` rs = fracCycle'' ([],r) r d
    | otherwise = fracCycle' (r:rs) r d
    where
        (q,r) = qr (10*n) d
fracCycle'' (qs,stopR) n d
    | r == stopR = reverse $ q:qs
    | otherwise = fracCycle'' (q:qs,stopR) r d
    where
        (q,r) = qr (10*n) d

qr n d = (q,r) where
    q = n `quot` d
    r = n `rem` d

main = print . fst . foldl1 max' . map cycleLength $ ns where
    cycleLength (i,n) = (i, length . fracCycle 1 $ n)
    max' ta@(ia,a) tb@(ib,b)
        | b > a = tb
        | otherwise = ta
    ns = zip [1..] [1..limit - 1]
    limit = 1000
