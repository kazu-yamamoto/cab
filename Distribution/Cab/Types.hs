module Distribution.Cab.Types where

type FunctionCommand = [String] -> [Option] -> IO ()

data Option = OptNoharm
            | OptRecursive
            | OptAll
            | OptInfo
            | OptFlag String
            | OptTest
            | OptHelp
            | OptBench
            deriving (Eq,Show)

getSandbox :: [Option] -> Maybe FilePath
getSandbox = undefined
