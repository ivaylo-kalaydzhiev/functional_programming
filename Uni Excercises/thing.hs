facts = 1 : zipWith (*) facts [1..]

-- >>> take 6 facts
-- [1,1,2,6,24,120]

-- >>> fst [12, 3]
-- Couldn't match expected type: (a_ayyt[sk:1], b0_ayyu[tau:1])
--             with actual type: [a0_ayyv[tau:1]]
-- In the first argument of `fst', namely `[12, 3]'
-- In the expression: fst [12, 3]
-- In an equation for `it_ayxe': it_ayxe = fst [12, 3]
-- Relevant bindings include
--   it_ayxe :: a_ayyt[sk:1]
--     (bound at C:\Users\ivayl\Desktop\thing.hs:5:2)

x = ([0],[[1]])

-- >>> x 4
-- (3,4)

-- >>> x == y
-- Couldn't match expected type: (Integer, Integer, Integer)
--             with actual type: ((Integer, Integer), Integer)
-- In the second argument of `(==)', namely `y'
-- In the expression: x == y
-- In an equation for `it_awli': it_awli = x == y
