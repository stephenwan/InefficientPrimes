module Math.Prime where

import Data.List

-- import Debug.Trace
-- debug = flip trace

data Seive = Seive { base :: Integer
                   , flr :: Integer
                   , ceil :: Integer
                   } deriving Show


forwardSeives' :: Integer -> [ Seive ] -> ([ Seive ], Bool)
forwardSeives' n [] = ([], False)
forwardSeives' n ( s : es) = 
  ( forward : forwardRest, matched || matchedRest)  
  where b = base s
        f = flr s
        c = ceil s
        (forward, matched) = ( if n <  c then s else s {flr = f + b, ceil = c + b}, n == c )
        (forwardRest, matchedRest ) = forwardSeives' n es

forwardSeives :: Integer -> [ Seive ] -> ([Seive], Bool)
forwardSeives n ss = let (ss', matchedAny) = forwardSeives' n ss
                         ns = Seive { base = n, flr =n , ceil = n + n}
                     in if matchedAny then (ss', matchedAny) else (ns : ss', matchedAny)
        


accumulate :: [ Integer ] -> [ Seive ]  -> [ Integer ]
accumulate [] _ = []
accumulate (n : ns) ss = let (ss', matched) = forwardSeives n ss
                         in  if matched then accumulate ns ss' else n : accumulate ns ss'


forwardSeives2' :: Integer -> [ Seive ] -> Maybe Integer -> ([ Seive ], Bool)
forwardSeives2' n [] _ = ([], False)
forwardSeives2' n ss Nothing = let thresh = (ceiling . sqrt . fromIntegral) n
                               in forwardSeives2' n ss (Just thresh)
forwardSeives2' n ss@(s:es) (Just thresh) =
  if base s > thresh
     -- `debug` ("\n -- Start " ++  (show n ) ++ (showList ss "") )
  then (s : forwardRest, matchedRest)
     -- `debug` ("\n -- ignored -- " ++ (showList ss ""))
  else let b = base s
           f = flr s
           c = ceil s           
           (forward, matched) =
             if n == c
             then (s {flr = c, ceil = c + b}, True )
             else if n < c
                  then (s, False)
                  else let q = n `quot` b * b
                       in (s {flr = q , ceil = q + b} , n == q )        
       in ( forward : forwardRest, matched || matchedRest )
          -- `debug` ("\n -- " ++ (showList forwardRest   $ show forward))
  where (forwardRest, matchedRest ) = forwardSeives2' n es (Just thresh)


forwardSeives2 :: Integer -> [ Seive ] -> ([Seive], Bool)
forwardSeives2 n ss = let (ss', matchedAny) = forwardSeives2' n ss Nothing
                          ns = Seive { base = n, flr =n , ceil = n + n}
                      in if matchedAny then (ss', matchedAny) else (ns : ss', matchedAny)
        
  
accumulate2 :: [ Integer ] -> [ Seive ]  -> [ Integer ]
accumulate2 [] _ = []
accumulate2 (n : ns) ss = let (ss', matched) = forwardSeives2 n ss
                          in  if matched then accumulate2 ns ss' else n : accumulate2 ns ss'
primes  = accumulate (2:3:((concat . transpose) [[5+6*x | x <- [0..]],[7+6*x | x <- [0..]]])) []
primes2 = accumulate2 (2:3:((concat . transpose) [[5+6*x | x <- [0..]],[7+6*x | x <- [0..]]])) []


                                                                      
