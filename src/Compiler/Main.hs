module Main where

import System.Environment
import System.IO
import System.Exit
import Data.List
import System.IO
import Control.Monad

import Compiler.CodeGeneration
import Compiler.ParseProgram
import Compiler.FunParser
import Compiler.TAMmt

main = do
    inp <- getArgs
    let arg = head inp
    if (isSuffixOf ".mt" arg)
        then do
            dat <- readFile arg
            let (Just index) = elemIndex '.' arg
            let path = fst(splitAt index arg) ++ ".tam"
            let ast = fst $ head $ parse progAST dat
            let code = genProg ast
            let string = writeTAM code
            writeFile path string
        else if (isSuffixOf ".tam" arg)
            then do
                let (Just index) = elemIndex '.' arg
                let path = fst(splitAt index arg) ++ ".obj"
                dat <- readFile arg
                let code = parseTAM dat
                let thing = assemble code
                let string = show thing
                writeFile path string
                return ()
        else do return ()
