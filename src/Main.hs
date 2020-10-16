-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           System.Environment (getArgs, getProgName)
import qualified System.IO          as IO
import           System.IO.Temp     (withSystemTempFile)
import           System.Process     (callProcess)

import           QuickdepFiles

-- | Prints usage help.
printUsage :: IO ()
printUsage =
    let usage progName =
            "USAGE:\n\
           \    " <> progName <> " [package] [dependencies]\n\
           \    package: name of temporary package\n\
           \    dependencies: list of dependencies to install temporarily"
     in do progName <- getProgName
           putStrLn $ usage progName

-- | Entry point that ensures we have at least 2 arguments.
main :: IO ()
main = getArgs >>= handleArgs
    where handleArgs []          = printUsage
          handleArgs [_]         = printUsage
          handleArgs (name:deps) = createPackage name deps

-- | Creates a package with given package name and dependencies, by appending
-- given information to the control file template and calling @equivs-build@.
createPackage :: String -> [String] -> IO ()
createPackage name deps = withSystemTempFile "control" f
    where
        f p h = do IO.hPutStr   h $ template -- copy template to file
                   IO.hPutStrLn h $ "Package: " <> name -- add package name
                   IO.hPutStrLn h $ "Depends:" -- add dependencies
                   IO.hPutStrLn h $ formatDeps deps
                   IO.hFlush h
                   callProcess "equivs-build" [p] -- call equivs-build

        -- Indents dependencies list
        formatDeps = unlines . map (' ':) . appendCommas

        -- Adds comma to each element in list except last
        appendCommas []     = []
        appendCommas [x]    = [x]
        appendCommas (x:xs) = (x <> ",") : appendCommas xs
