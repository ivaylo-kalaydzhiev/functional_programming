-- n permutation is pi: {0 .. n-1} -> {0 .. n-1}, pi is bijection
-- cycle is (x1, x2, .. , xk), such that
-- pi(x1) = x2, pi(x2) = x3, .. , pi(xk) = x1

-- actually I just need to check that the functional values contain
-- all of [0 .. n-1]
-- this would mean it is a surjection
-- it would also mean it is an inection, because with n unique values 
--    I will have created n unique values
-- it will also show that f's Domain is {0 .. n-1}

-- 1

-- a
quickSort [] = []
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
    where
        left = filter (<=x) xs
        right = filter (>x) xs

setEq s1 s2 = quickSort s1 == quickSort s2
isNPerm n f = [0 .. n-1] `setEq` map f [0 .. n-1]

-- b
