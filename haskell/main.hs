module Main where

import Sistema (runSistema)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runSistema
