{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude       hiding ( readFile )

import Crypto.Hash
import Options.Applicative
import Text.Printf          ( printf )
import Data.ByteString      ( ByteString, readFile )
import System.Directory     ( listDirectory, getPermissions, Permissions (readable), removeFile, doesFileExist )
import Control.Monad        ( unless, filterM )
import Control.Exception    ( try, SomeException )



main :: IO ()
main = do
    args <- execParser opts
    
    ep <- try (getPermissions (path args)) :: IO (Either SomeException Permissions)
    case ep of
        Left e -> do
            printf "Path \"%s\" does not exist or is unreadable\n%s" (path args) (show e)
            return ()

        Right p -> unless (readable p) $ do
            printf "No Permission to read \"%s\"" (path args)
            return ()
    
    putStrLn "before composeHashes"
    hashes <- composeHashes (recursive args) (path args)

    deleteDuplicates hashes
    where
        opts :: ParserInfo Args
        opts = info (argParse <**> helper)
            ( fullDesc
           <> progDesc "CLI tool for removing duplicate media"
            )



deleteDuplicates :: [(FilePath, Digest SHA256)] -> IO ()
deleteDuplicates []     = pure ()
deleteDuplicates ((xa, xb):xs)
    | any (\(_, b) -> xb == b) xs   = removeFile xa >> deleteDuplicates xs
    | otherwise                     = deleteDuplicates xs


composeHashes :: Bool -> FilePath -> IO [(FilePath, Digest SHA256)]
composeHashes False p = do
    ps <- listDirectory p
    fs <- filterM doesFileExist (map (joinPath p) ps)

    mapM (\x -> hashFile x >>= \z -> return (x, z)) fs
composeHashes True p = do
    ps <- listDirectory p
    (fs, ds) <- partitionM doesFileExist (map (joinPath p) ps)

    as <- mapM (\x -> hashFile x >>= \z -> return (x, z)) fs
    bs <- mapM (composeHashes True . joinPath p) ds

    return (as ++ concat bs)



partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []     = pure ([], [])
partitionM f (x:xs) = do
    res     <- f x
    (a, b)  <- partitionM f xs
    pure ([x | res] ++ a, [x | not res] ++ b)


joinPath :: FilePath -> FilePath -> FilePath
joinPath x y = x <> "/" <> y


sha256 :: ByteString -> Digest SHA256
sha256 = hash


hashFile :: FilePath -> IO (Digest SHA256)
hashFile = fmap sha256 . readFile



argParse :: Parser Args
argParse = Args
    <$> strArgument
        ( metavar "path"
       <> help "Scanning Path"
        )
    <*> switch
        ( long "recursive"
       <> short 'r'
       <> help "Search recursively"
        )


data Args = Args
    { path      :: String
    , recursive :: Bool
    }
