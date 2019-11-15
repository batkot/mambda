module Main where

main :: IO ()
main = mapM_ putStrLn snake

snake :: [String]
snake = 
    [ "       __"
    , "      {0O}"
    , "      \\__/"
    , "      /^/"
    , "     ( (              -Peter Bier-"
    , "     \\_\\_____"
    , "     (_______)"
    , "    (_________()Oo"
    ]
