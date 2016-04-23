module Util.List
( chunks
) where

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (ys, zs) = splitAt n xs
              in ys : chunks n zs
