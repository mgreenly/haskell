-- https://github.com/pcapriotti/optparse-applicative
--
--  mkdeb
--    --package=$(basename $PWD)
--    --version=
--    --architecture=$(dpkg --print-architecture)
--    --maintainer="$(git config user.name) <$(git config user.email)>"
--    --description="no description given"
--    --dep=lib
--    --data=src:dst
--    --bin=src:dst
--

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Process


data Options = Options
    { optionsBin :: [String]
    , optionsData :: [String]
    , optionsDep :: [String]
    }


options :: Parser Options
options = Options
    <$> some binOpt
    <*> some dataOpt
    <*> many depOpt
    where
        binOpt =
            strOption (
                long "bin"  <>
                metavar "src:dst"  <>
                help "executable files to add"
            )

        dataOpt =
            strOption (
                long "data" <>
                metavar "src:dst" <>
                help "other files to add"
            )

        depOpt =
            strOption (
                long "dep" <>
                metavar "lib" <>
                help "specify library dependency"
            )


main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc
            <> progDesc "Print a greeting for"
            <> header "mkdeb - a simple *.deb construction tool"
        )

run :: Options -> IO ()
run (Options x y z) = do
    print x
    print y
    print z
    result <- readProcess "seq" ["1", "10"] ""
    print result
