module Useful where

import Data.Char(isDigit)

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if t==x then 0 else 1 + getIndex t xs

sepChar :: Char -> String -> [String]
sepChar _ [] = []
sepChar ch [x]    = if x==ch then [[]] else [[x]]
sepChar ch (x:xs) = if x==ch then []:(hd:tl)
                             else (x:hd):tl
                          where (hd:tl) = sepChar ch xs

joinChar :: Char -> [String] -> String
joinChar _ [] = []
joinChar _ [x] = x
joinChar ch (x:xs) = x++[ch]++joinChar ch xs

dsort :: Ord a => [(a,b)] -> [b] 
dsort dt = map snd (sorting dt)

sorting :: Ord a => [(a,b)] -> [(a,b)]
sorting [] = []
sorting ((a,b):xs) = sorting sml ++ [(a,b)] ++ sorting lar
   where sml = [(p,n) | (p,n) <- xs, p<a]
         lar = [(q,m) | (q,m) <- xs, q>=a]

toList :: (Enum a,Ord a) => a -> a -> [a]
toList a b 
  | a==b = [a]
  | a<b = [a..b]
  | otherwise = [b..a]

isNum :: String -> Bool
isNum = foldr ((&&).isDigit) True 

isChar :: String -> String -> Bool
isChar [] _ = True 
isChar (x:xs) str = x `elem` str && isChar xs str

isStr :: String -> Bool
isStr = foldr (\x -> (&&) (not$isDigit x)) True 

chooseData :: String -> [String] -> [String]
chooseData _ [] = []
chooseData h (x:xs) =
  let lh = length h
   in if take lh x==h then drop lh x : chooseData h xs else chooseData h xs

replCon :: Int -> a -> [a] -> [a]
replCon i x y = take i y ++ [x] ++ drop (i+1) y 

delCon :: Int -> [a] -> [a]
delCon i y = take i y ++ drop (i+1) y 

dataSub :: Eq a => [a] -> [a] -> [a]
dataSub org [] = org 
dataSub org (t:ts) =
  let ie = elem t org
      i = if ie then getIndex t org else (-1)
   in if i>(-1) then dataSub (delCon i org) ts
                  else dataSub org ts

dataAdd :: Eq a => [a] -> [a] -> [a]
dataAdd org [] = org 
dataAdd org (t:ts) =
  let ie = elem t org
   in if ie then dataAdd org ts
            else dataAdd (org++[t]) ts

isTime :: String -> Bool
isTime str =
  let ie = elem ' ' str
      els = if ie then sepChar ' ' str else []
      lng = if ie then length els else 0
   in (lng==2 && isNum (head els) && isNum (els!!1)) 

calcTime :: Int -> IO Int
calcTime i = do
  inp <- putStr "> " >> getLine
  let ia = isTime inp
      [min,sec] = if ia then sepChar ' ' inp else ["0","0"] 
  if ia then do
              let asec = read min * 60+ read sec + i
                  (rmin,rsec) = (div asec 60, mod asec 60)
              putStrLn (show rmin ++ ":" ++ show rsec)
              calcTime asec
        else if inp=="q" then return i else do
              putStrLn "Error! Enter Time Again"
              calcTime i 
        
