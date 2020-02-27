

startsWith _ [] = True
startsWith [] _ = False
startsWith (x:xs) (y:ys) = 
    if x==y then 
        xs `startsWith` ys
    else False

fractionDecimalPart n d = tail $  dropWhile (/='.') (show (n/d))

findRecurringCycle (c:cs) = helper (c:[]) cs where
    helper cycle rem =
        if rem `startsWith` cycle  then
            helper cycle $ drop (length cycle) rem
        else False

res = False