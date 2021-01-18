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
    r <- randomRIO (0, 3)
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
 
process :: Name -> Customer -> MVar Customer -> MVar Value -> MVar Customer -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> IO () 
process name customer mvar value customerlist balance1 balance2 balance3 balance4 = do
    c1 <- coinFlip
    putStrLn $ name ++ "'s turn, they -- got " ++ (show c1) 
       
    if c1 == Head then do
        putMVar mvar customer
        putMVar customerlist customer
        r2 <- randomCustIndex
        putStrLn $ name ++ " -- got " ++ (show r2)
        putMVar value r2
        if r2 == 0 then do 
            number <- takeMVar balance1
            let newnumber = number + 10
            putMVar balance1 newnumber
        else if r2 == 1 then do
          number <- takeMVar balance2
          let newnumber = number + 20
          putMVar balance2 newnumber
        else if r2 == 2 then do
          number <- takeMVar balance3
          let newnumber = number + 30
          putMVar balance3 newnumber 
         else do  
            number <- takeMVar balance4
            let newnumber = number + 40
            putMVar balance4 newnumber    
    
    else do    

        randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
        process name customer mvar value customerlist balance1 balance2 balance3 balance4
    
    
-- MAIN FUNCTION        
main :: IO ()
main = do
    balance1 <- newMVar 1000
    balance2 <- newMVar 1000
    balance3 <- newMVar 1000
    balance4 <- newMVar 1000
    putStrLn $ ".******------ WELCOME ------******."   
    let c1 = Customer {name = "C1", balance = balance1, account = 1}
    let c2 = Customer {name = "C2", balance = balance2, account = 2} 
    let c3 = Customer {name = "C3", balance = balance3, account = 3}
    let c4 = Customer {name = "C4", balance = balance4, account = 4} 
    putStrLn $ ".******------ CUSTOMERS CREATED ------******." 
    
    forM_ [1..1] $ \_ -> do
        
  
    
    -- MVars for customers
       one <- newEmptyMVar
       two <- newEmptyMVar
       three <- newEmptyMVar
       four <- newEmptyMVar
    
    -- MVars for index values
       value1 <- newEmptyMVar
       value2 <- newEmptyMVar
       value3 <- newEmptyMVar
       value4 <- newEmptyMVar
    
       customerlist <- newEmptyMVar
       -- MERGING tests with 'b'
       b <- newEmptyMVar
       c <- newEmptyMVar
       putStrLn $ ".******------ EMPTY MVARS CREATED ------******."
       randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
       mapM_ forkIO [process "C1" c1 one value1 customerlist balance1 balance2 balance3 balance4, process "C2" c2 two value2 customerlist balance1 balance2 balance3 balance4, process "C3" c3 three value3 customerlist balance1 balance2 balance3 balance4, process "C4" c4 four value4 customerlist balance1 balance2 balance3 balance4]
       putStrLn $ ".******------ THREADS RUNNING ------******."

    -- haven't used this
       usecustomers <- newMVar [one, two , three, four]

    
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


   -- INDEX value tests | unblock -- this way we can read each value each customer thread got
       rvalue1 <- readMVar value1
       putStrLn $ show rvalue1

       rvalue2 <- readMVar value2
       putStrLn $ show rvalue2

       rvalue3 <- readMVar value3
       putStrLn $ show rvalue3

       rvalue4 <- readMVar value4
       putStrLn $ show rvalue4


    
-- || INDEXING FOR TRANSFERS   
       c <- takeMVar usecustomers -- c :: [MVar Customer]
    
       let index = (c!!rvalue1)
       z <- readMVar index 
       n <- randomAmount
       putStrLn $ "RANDOM AMOUNT: " ++ show n
       

       let index2 = (c!!rvalue2)
       y <- readMVar index2

       let index3 = (c!!rvalue3)
       w <- readMVar index3

       let index4 = (c!!rvalue4)
       v <- readMVar index4
       

       bal1 <- readMVar  balance1
       print bal1
    
       bal2 <- readMVar  balance2
       print bal2

       bal3 <- readMVar  balance3
       print bal3

       bal4 <- readMVar balance4
       print bal4


       putStrLn $ ".******------ TEST || THREADS ALL RUN - EXIT ------******."
    
    

