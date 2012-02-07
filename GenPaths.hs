{-# LANGUAGE OverloadedStrings #-}

module GenPaths (genPaths) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Enumerator
import Data.Enumerator (run, ($$))
import Data.Enumerator.Binary (enumFile)
import Data.List
import System.Directory
import System.IO

genPaths :: IO ()
genPaths = do
    mnv <- getNameVersion
    case mnv of
        Nothing       -> hPutStrLn stderr "cabal file does not exist"
        Just (nm,ver) -> do
            let file = "Paths_" ++ nm' ++ ".hs"
            exist <- doesFileExist file
            if exist then
                hPutStrLn stderr $ file ++ " already exists"
              else do
                putStrLn $ "Writing " ++ file ++ "..."
                writeFile file $
                     "module Paths_" ++ nm' ++ "  where\n"
                  ++ "import Data.Version\n"
                  ++ "\n"
                  ++ "version :: Version\n"
                  ++ "version = Version [" ++ ver' ++ "] []\n"
          where
            nm'  = map (trans '-' '_') nm
            ver' = map (trans '.' ',') ver
            trans c1 c2 c
              | c == c1   = c2
              | otherwise = c

getNameVersion :: IO (Maybe (String,String))
getNameVersion = do
    mcfile <- getCabalFile
    case mcfile of
        Nothing -> return Nothing
        Just cfile -> do
            mn <- parseCabalFile cfile name
            mv <- parseCabalFile cfile version
            return $ do
                n <- mn
                v <- mv
                return (n,v)

getCabalFile :: IO (Maybe FilePath)
getCabalFile = do
    cnts <- (filter isCabal <$> getDirectoryContents ".")
            >>= filterM (\file -> doesFileExist file)
    case cnts of
        []      -> return Nothing
        cfile:_ -> return (Just cfile)
  where
    isCabal nm = ".cabal" `isSuffixOf` nm && length nm > 6

parseCabalFile :: FilePath -> Parser a -> IO (Maybe a)
parseCabalFile file parser = do
    res <- run (enumFile file $$ iterParser (findTarget parser))
    case res of
        Right x -> return x
        Left  _ -> return Nothing

findTarget :: Parser a -> Parser (Maybe a)
findTarget parser = (Just <$> parser)
         <|> (anyChar >> findTarget parser)
         <|> (Nothing <$ endOfInput)

name :: Parser String
name = do
    stringCI "name:"
    many (char ' ')
    many1 (satisfy $ notInClass " ,\t\n")

version :: Parser String
version = do
    stringCI "version:"
    many (char ' ')
    many1 (satisfy $ inClass "0-9.")
