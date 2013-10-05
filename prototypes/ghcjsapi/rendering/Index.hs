{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

import JavaScript.JQuery
import Interactive


main = do
    test <- select "#test"
    act <- runInteractive test f
    btn <- select "<button>Go</button>" >>= appendToJQuery test
    onClick btn $ \_ -> act
    -- runI test f
    return ()

f :: Int -> String -> Int
f x s = x + length s
