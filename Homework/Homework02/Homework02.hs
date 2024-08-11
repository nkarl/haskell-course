
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Floating a => a -> a -> a -> a
f1 x y z = x ** (y/z)

f2 :: Floating a => a -> a -> a -> a
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
-- Helps us understand the input and output

-- Question 3
-- Why should you define type signatures for variables? How can they help you?
-- Helps us understand the type of the variable.


-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
-- Yes, we use a lift or a transformer.

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
-- Yes, it's a list of String (which is a list of Char). To access the innemost elements, we should
-- use a function that pattern match the outer list, unpack it and access the first-ordered element
-- and then we can use `where` or `let` to define a closure to access the second-ordered elements.
