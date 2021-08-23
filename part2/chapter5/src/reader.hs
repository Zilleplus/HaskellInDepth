{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader

data Config = Config{
    verbose :: Bool,
    age :: Int
  }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config { verbose = True , age = 3}

work :: ConfigM ()
work = do doSomething

doSomething :: ConfigM ()
doSomething = do
  doSomethingSpecial

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
  -- Config {verbose, age} <- ask
  vrb <- asks verbose
  when vrb beVerbose

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result
