module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import Control.Monad (forM_, replicateM_ )
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)


-- DATA TYPES
data Customer = Customer {
  name :: Name,
  balance :: MVar Balance,
  account :: Account
} deriving (Eq)
 
type Account = Int
type Balance =  Int
type Name = String
type Value = Int

data Coin = Head | Tail deriving (Show, Eq)  

-- RANDOM GENERATOR FUNCTIONS
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

randomCustIndex :: IO Int 
randomCustIndex = do
    r <- randomRIO (0, 7)
    return r    

randomAmount :: IO Int 
randomAmount = do
    r <- randomRIO (10, 50)
    return r    

{-
-- TRANSFER FUNCTION
transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  | amount <= 0 = return (from, to)
  | balance from < amount = return (from, to) -- maybe remove this
  | balance from <= 0  = return (from, to) -- i added this to try to fit r'qs -- a transaction is processed but no money is passed if account balance is 0
  | name from == name to = return (from, to)  -- i added this to try to fit r'qs -- customers to return even if it's the same account -- though we have caught these cases in main
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))
-}

-- THREAD PROCESS
-- INDEX --
-- C1 balance1 = 0 
-- C2 balance2 = 1
-- C3 balance3 = 2
-- C4 balance4 = 3

 
process :: Name -> Customer -> MVar Customer -> MVar Value -> MVar Customer -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> IO () 
process name customer mvar value customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 = do
 {- forM_ [1..3] $ \_ -> do -}
    c1 <- coinFlip
    putStrLn $ name ++ "'s turn, they -- got " ++ (show c1)    
    if c1 == Head then do
        putMVar mvar customer
        putMVar customerlist customer
        r1 <- randomAmount
        r2 <- randomCustIndex
        putStrLn $ name ++ " -- got " ++ (show r2) ++ "-- and random amount is -- " ++ (show r1)
        putMVar value r2

        if r2 == 0 then do 
            number <- takeMVar balance1
            let newnumber = number + r1
            putMVar balance1 newnumber
        ----------------------------------- 1 attempt at withdrawals
            if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber  
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber     
              else do 
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
        ----------------------------------- 1 attempt at withdrawals    


        else if r2 == 1 then do
            number <- takeMVar balance2
            let newnumber = number + r1
            putMVar balance2 newnumber
          --------------------------- 2 attempt at withdrawals
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber  
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber         
              else do 
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber       
        ---------------------------------- 2 attempt at withdrawals  


        else if r2 == 2 then do
          number <- takeMVar balance3
          let newnumber = number + r1
          putMVar balance3 newnumber 
        --------------------------- 3 attempt at withdrawals
          if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
          else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
          else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
          else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
          else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber 
          else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber   
          else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber          
            else do 
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber       
        ---------------------------------- 3 attempt at withdrawals 


        else if r2 == 3 then do
            number <- takeMVar balance4
            let newnumber = number + r1
            putMVar balance4 newnumber    
        --------------------------- 4 attempt at withdrawals
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber             
              else do 
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber       
        ---------------------------------- 5 attempt at withdrawals 
        else if r2 == 4 then do
            number <- takeMVar balance5
            let newnumber = number + r1
            putMVar balance5 newnumber  
      --------------------------- 5 attempt at withdrawals
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber   
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber          
              else do 
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber    
        ---------------------------------- 6 attempt at withdrawals
        else if r2 == 5 then do
            number <- takeMVar balance6
            let newnumber = number + r1
            putMVar balance6 newnumber 
        ---------------------------------- 6 attempt at withdrawals    
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber  
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber           
              else do 
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
        ---------------------------------- 7 attempt at withdrawals
        else if r2 == 6 then do
            number <- takeMVar balance7
            let newnumber = number + r1
            putMVar balance7 newnumber 
        ---------------------------------- 7 attempt at withdrawals
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber  
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber                
              else do 
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber
        ---------------------------------- 8 attempt at withdrawals
          else do 
            number <- takeMVar balance8
            let newnumber = number + r1
            putMVar balance8 newnumber 
        ---------------------------------- 8 attempt at withdrawals         
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber                  
              else do 
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber
    else do    

        randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
        process name customer mvar value customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8
    
    
-- MAIN FUNCTION        
main :: IO ()
main = do
    balance1 <- newMVar 1000
    balance2 <- newMVar 1000
    balance3 <- newMVar 1000
    balance4 <- newMVar 1000
    balance5 <- newMVar 1000
    balance6 <- newMVar 1000
    balance7 <- newMVar 1000
    balance8 <- newMVar 1000
    putStrLn $ ".******------ WELCOME ------******."   
    let c1 = Customer {name = "C1", balance = balance1, account = 1}
    let c2 = Customer {name = "C2", balance = balance2, account = 2} 
    let c3 = Customer {name = "C3", balance = balance3, account = 3}
    let c4 = Customer {name = "C4", balance = balance4, account = 4} 
    let c5 = Customer {name = "C5", balance = balance5, account = 5}
    let c6 = Customer {name = "C6", balance = balance6, account = 6}
    let c7 = Customer {name = "C7", balance = balance7, account = 7}
    let c8 = Customer {name = "C8", balance = balance8, account = 8}
    putStrLn $ ".******------ CUSTOMERS CREATED ------******." 
    
    ---- ADD 10x transactions here and it works
    forM_ [1..10] $ \_ -> do
        
  
    
    -- MVars for customers
       one <- newEmptyMVar
       two <- newEmptyMVar
       three <- newEmptyMVar
       four <- newEmptyMVar
       five <- newEmptyMVar
       six <- newEmptyMVar
       seven <- newEmptyMVar
       eight <- newEmptyMVar
    
    -- MVars for index values
       value1 <- newEmptyMVar
       value2 <- newEmptyMVar
       value3 <- newEmptyMVar
       value4 <- newEmptyMVar
       value5 <- newEmptyMVar
       value6 <- newEmptyMVar
       value7 <- newEmptyMVar
       value8 <- newEmptyMVar
    
       customerlist <- newEmptyMVar
       -- MERGING tests with 'b'
       
       putStrLn $ ".******------ EMPTY MVARS CREATED ------******."
       randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
       mapM_ forkIO [process "C1" c1 one value1 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8, process "C2" c2 two value2 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8, process "C3" c3 three value3 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8, process "C4" c4 four value4 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8, process "C5" c5 five value5 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8, process "C6" c6 six value6 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8, process "C7" c7 seven value7 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8, process "C8" c8 eight value8 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8]
       putStrLn $ ".******------ THREADS RUNNING ------******."

    -- haven't used this
       usecustomers <- newMVar [one, two , three, four, five, six , seven, eight]

    
       firsthead <- takeMVar customerlist
       let print_name = print . name
       let reveal_first = print_name firsthead
       putStrLn $ "FIRST HEAD: "  
       reveal_first

       secondhead <- takeMVar customerlist
       let reveal_second = print_name secondhead
       putStrLn $ "SECOND HEAD: "  
       reveal_second
    
       thirdhead <- takeMVar customerlist
       let reveal_third = print_name thirdhead
       putStrLn $ "THIRD HEAD: "  
       reveal_third
    
       fourthhead <- takeMVar customerlist
       let reveal_fourth = print_name fourthhead
       putStrLn $ "FOURTH HEAD: "  
       reveal_fourth

       fifthhead <- takeMVar customerlist
       let reveal_fifth = print_name fifthhead
       putStrLn $ "FIFTH HEAD: "  
       reveal_fifth

       sixthhead <- takeMVar customerlist
       let reveal_sixth = print_name sixthhead
       putStrLn $ "SIXTH HEAD: "  
       reveal_sixth

       seventhhead <- takeMVar customerlist
       let reveal_seventh = print_name seventhhead
       putStrLn $ "SEVENTH HEAD: "  
       reveal_seventh

       eighthhead <- takeMVar customerlist
       let reveal_eighth = print_name eighthhead
       putStrLn $ "EIGHTH HEAD: "  
       reveal_eighth 


   -- INDEX value tests | unblock -- this way we can read each value each customer thread got
       rvalue1 <- readMVar value1
       putStrLn $ show rvalue1

       rvalue2 <- readMVar value2
       putStrLn $ show rvalue2

       rvalue3 <- readMVar value3
       putStrLn $ show rvalue3

       rvalue4 <- readMVar value4
       putStrLn $ show rvalue4

       rvalue5 <- readMVar value5
       putStrLn $ show rvalue5

       rvalue6 <- readMVar value6
       putStrLn $ show rvalue6

       rvalue7 <- readMVar value7
       putStrLn $ show rvalue7

       rvalue8 <- readMVar value8
       putStrLn $ show rvalue8
    
-- || INDEXING FOR TRANSFERS   
       c <- takeMVar usecustomers -- c :: [MVar Customer]
    
       let index = (c!!rvalue1)
       z <- readMVar index 
       n <- randomAmount
       --putStrLn $ "RANDOM AMOUNT: " ++ show n
       

       let index2 = (c!!rvalue2)
       y <- readMVar index2

       let index3 = (c!!rvalue3)
       w <- readMVar index3

       let index4 = (c!!rvalue4)
       v <- readMVar index4

       let index5 = (c!!rvalue5)
       v <- readMVar index5

       let index6 = (c!!rvalue6)
       v <- readMVar index6

       let index7 = (c!!rvalue7)
       v <- readMVar index7

       let index8 = (c!!rvalue8)
       v <- readMVar index8

       

       bal1 <- readMVar  balance1
       print bal1
    
       bal2 <- readMVar  balance2
       print bal2

       bal3 <- readMVar  balance3
       print bal3

       bal4 <- readMVar balance4
       print bal4

       bal5 <- readMVar  balance5
       print bal5

       bal6 <- readMVar balance6
       print bal6

       bal7 <- readMVar  balance7 
       print bal7

       bal8 <- readMVar balance8
       print bal8

       putStrLn $ ".******------ TEST || THREADS ALL RUN - EXIT ------******."
    
    

