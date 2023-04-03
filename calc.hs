
myAdd :: Int -> Int -> Int
myAdd a 0 = a
myAdd a b = myAdd (a + 1) (b - 1)

mySupp :: Int -> Int -> Int
mySupp a 0 = a
mySupp a b = mySupp (a - 1) (b - 1)

myMul :: Int -> Int -> Int
myMul _ 0 = 0 
myMul a b = a + myMul a  (b - 1)

myDiv :: Int -> Int -> Int
myDiv 0 _ = 0
myDiv a b  = 1 + myDiv (a - b) b

main = do
       putStr "\n\n"
       putStrLn "enter value for x: "
       inp1 <- getLine
       putStrLn "which operation?:" 
       inp3 <- getLine
       putStrLn "enter value for y: " 
       inp2 <- getLine
       let x = (read inp1 :: Int)
       let y = (read inp2 :: Int)
       let symbol = inp3
       if symbol == "+"
          then do
              let res = myAdd x y
              putStrLn "RESULT: "
              print res
              main

       else if symbol == "-"
          then do 
              let res = mySupp x y
              putStrLn "RESULT: "
              print res
              main

       else if symbol == "*"
          then do 
              let res = myMul x y
              putStrLn "RESULT: "
              print res
              main
       
       else if symbol == "/"
          then do 
              let res = myDiv x y
              putStrLn "RESULT: "
              print res
              main
       else if symbol == "exit"
          then do
              putStrLn "exit"
       else
              putStrLn "wrong input"