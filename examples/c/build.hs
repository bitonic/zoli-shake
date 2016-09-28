{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Control.Monad (void)
import           Data.Functor.Identity (Identity(..))
import           System.FilePath (takeDirectory)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Development.Shake as Shake

import           Zoli

build :: (Pattern tok, MonadIO m) => Rules tok m Identity ()
build = do
  objects <- rule [pt|objs/*.o|] $ \name out -> do
    let source = [pt|src/*.c|] @@ name
    needFiles [source]
    let deps = [pt|deps/*.d|] @@ name
    mkdirP (takeDirectory deps)
    () <- cmd_ "cc" ["-MD", "-MF", deps, "-c", "-o", out, source]
    -- The first string is the rule target, which we don't need.
    _ : depsFiles <- filter (\w -> w /= source && w /= "\\") . words <$> liftIO (readFile deps)
    needFiles depsFiles

  bin <- rule [pt|bin/myprog|] $ \() out -> do
    let names = ["a", "b"]
    objectsFiles <- needToks (map (objects,) names)
    cmd_ "cc" (["-o", out] ++ objectsFiles)

  want [Tok bin ()]

  void $ phony "clean" $ do
    rmDir "objs"
    rmDir "deps"

main :: IO ()
main = Shake.shakeArgs Shake.shakeOptions (runIdentity (mkRules build))
