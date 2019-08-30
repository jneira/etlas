{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings #-}
module Distribution.Client.PackageDescription.Dhall where

import Control.Exception ( throwIO )

import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )

import qualified Data.Text as StrictText
import qualified Data.Text.IO as StrictText

import qualified Dhall
import qualified Dhall.Binary as Dhall
import qualified Dhall.Core as Dhall
  hiding ( Type )
import qualified Dhall.Context
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import qualified Dhall.Format as Dhall
import qualified Dhall.Freeze as Dhall
import qualified Dhall.Pretty  as Dhall ( CharacterSet(..) )

import DhallToCabal ( genericPackageDescription  ) 
import qualified CabalToDhall as Dhall ( cabalToDhall )
import DhallLocation ( dhallFromGitHub )
import Distribution.Verbosity
import Distribution.PackageDescription.PrettyPrint
       ( writeGenericPackageDescription )
#ifdef CABAL_PARSEC
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Distribution.PackageDescription.Parsec as Cabal.Parse
       ( readGenericPackageDescription
       , parseGenericPackageDescriptionMaybe
       ) 
#else
import Distribution.PackageDescription.Parse as Cabal.Parse
       ( readGenericPackageDescription
       , parseGenericPackageDescription
       , ParseResult(..)
       )
#endif
import Distribution.Simple.Utils
       ( info
       , createDirectoryIfMissingVerbose
       )
import Distribution.PackageDescription
import Distribution.Types.Dependency
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree

import qualified Lens.Micro as Lens
import Lens.Micro ( Lens' )
import qualified Lens.Micro.Extras as Lens

import System.CPUTime ( getCPUTime )
import System.FilePath
       ( takeDirectory
       , takeExtension
       )
                
readGenericPackageDescription :: Verbosity -> FilePath
                              -> IO GenericPackageDescription
readGenericPackageDescription verbosity path =
  if (takeExtension path) == ".dhall" then
    readAndParse verbosity path
  else
    Cabal.Parse.readGenericPackageDescription verbosity path

parseCabalGenericPackageDescription :: String
                                    -> Maybe GenericPackageDescription
#ifdef CABAL_PARSEC
parseCabalGenericPackageDescription content =
        Cabal.Parse.parseGenericPackageDescriptionMaybe $ BS.Char8.pack content
#else
parseCabalGenericPackageDescription content =
      case Cabal.Parse.parseGenericPackageDescription content of
        ParseOk _ pkg -> Just pkg
        _             -> Nothing
#endif


measuringTime :: Verbosity -> String -> IO a -> IO a
measuringTime verbosity msg action = do
  start <- getCPUTime
  x <- action
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^(12 :: Integer))
  info verbosity $ msg ++ show (diff :: Double) ++ " seconds"
  return x
  
readAndParse :: Verbosity -> FilePath -> IO GenericPackageDescription
readAndParse verbosity dhallFilePath = do
  let explaining = if verbosity >= verbose then Dhall.detailed else id
  info verbosity $ "Reading and parsing package configuration from dhall file: "
                ++ dhallFilePath
  measuringTime verbosity "Configuration readed in " $ do
    src <- StrictText.readFile dhallFilePath
    explaining $ parse dhallFilePath src

parse :: FilePath -> StrictText.Text -> IO GenericPackageDescription
parse dhallFilePath src = compileDhall dhallFilePath src >>= extract

compileDhall :: FilePath -> StrictText.Text -> IO ( Dhall.Expr Dhall.Src Dhall.X ) 
compileDhall dhallFilePath src = do
  let  settings = Dhall.defaultInputSettings
         & Lens.set Dhall.rootDirectory ( takeDirectory dhallFilePath )
         & Lens.set Dhall.sourceName dhallFilePath

  expr  <- Dhall.inputExprWithSettings settings src

  return $ Dhall.alphaNormalize expr

extract :: Dhall.Expr Dhall.Src Dhall.X -> IO GenericPackageDescription
extract expr = do
  let Dhall.Type {..} = genericPackageDescription
      annot = ( ( Dhall.Annot expr expected )
                :: Dhall.Expr Dhall.Src Dhall.X )

  _ <- throws ( Dhall.typeWith Dhall.Context.empty annot )

  return $ fixGPDConstraints ( either
                               ( error . show ) id
                               ( Dhall.toMonadic ( extract expr ) ) )

  where throws = either Control.Exception.throwIO return

writeDerivedCabalFile :: Verbosity -> FilePath
                      -> GenericPackageDescription -> IO ()
writeDerivedCabalFile verbosity path genPkg = do
  info verbosity $ "Writing derived cabal file from dhall file: " ++ path

  let dir = takeDirectory path

  createDirectoryIfMissingVerbose verbosity True dir
  writeGenericPackageDescription path genPkg

writeAndFreezeCabalToDhall :: Verbosity -> FilePath -> String -> IO ()
writeAndFreezeCabalToDhall verbosity path cabal = do
  info verbosity $ "Writing dhall file: " ++ path
  StrictText.writeFile path ( cabalToDhall cabal )
  info verbosity $ "Formatting dhall file..."
  Dhall.format (Dhall.Format Dhall.Unicode ( Dhall.Modify ( Just path ) ))
  info verbosity $ "Freezing dhall file..."
  Dhall.freeze ( Just path ) Dhall.AllImports Dhall.Secure Dhall.Unicode
               Dhall.defaultStandardVersion 
  
cabalToDhall :: String -> Dhall.Text
cabalToDhall cabal = Dhall.pretty dhallExpr
  where gpd = fromMaybe
                ( error "Unable to parse cabal content" ) 
                ( parseCabalGenericPackageDescription cabal )
        dhallExpr = Dhall.cabalToDhall dhallFromGitHub  gpd


-- TODO: Pick Lens modules from Cabal if we need them in more places
condLibrary' :: Lens' GenericPackageDescription (Maybe (CondTree ConfVar [Dependency] Library))
condLibrary' f s = fmap (\x -> s { condLibrary = x }) (f (condLibrary s))

condSubLibraries' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] Library))]
condSubLibraries' f s = fmap (\x -> s { condSubLibraries = x }) (f (condSubLibraries s))

condForeignLibs' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] ForeignLib))]
condForeignLibs' f s = fmap (\x -> s { condForeignLibs = x }) (f (condForeignLibs s))

condExecutables' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] Executable))]
condExecutables' f s = fmap (\x -> s { condExecutables = x }) (f (condExecutables s))

condTestSuites' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] TestSuite))]
condTestSuites' f s = fmap (\x -> s { condTestSuites = x }) (f (condTestSuites s))

condBenchmarks' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] Benchmark))]
condBenchmarks' f s = fmap (\x -> s { condBenchmarks = x }) (f (condBenchmarks s))

fixGPDConstraints
  :: GenericPackageDescription
  -> GenericPackageDescription
fixGPDConstraints
  = Lens.over ( condBenchmarks' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condExecutables' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condForeignLibs' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condLibrary' . traverse ) fixCondTreeConstraints
  . Lens.over ( condSubLibraries' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condTestSuites' . traverse . Lens._2 ) fixCondTreeConstraints

class HasBuildInfo a where
   buildInfo' :: Lens' a BuildInfo
   targetBuildDepends' :: Lens' a [Dependency]
   targetBuildDepends' = buildInfo' . targetBuildDepends'

instance HasBuildInfo BuildInfo where
   buildInfo' = id
   targetBuildDepends' f s = fmap (\x -> s { targetBuildDepends = x }) (f (targetBuildDepends s))

instance HasBuildInfo Benchmark where
    buildInfo' f (Benchmark x1 x2 x3) = fmap (\y1 -> Benchmark x1 x2 y1) (f x3)

instance HasBuildInfo Executable where
    buildInfo' f l = (\x -> l { buildInfo = x }) <$> f (buildInfo l)

instance HasBuildInfo ForeignLib where
    buildInfo' f l = (\x -> l { foreignLibBuildInfo = x }) <$> f (foreignLibBuildInfo l)

instance HasBuildInfo Library where
    buildInfo' f l = (\x -> l { libBuildInfo = x }) <$> f (libBuildInfo l)

instance HasBuildInfo TestSuite where
    buildInfo' f l = (\x -> l { testBuildInfo = x }) <$> f (testBuildInfo l)

fixCondTreeConstraints
  :: ( HasBuildInfo a )
  => CondTree v cs a
  -> CondTree v [Dependency] a
fixCondTreeConstraints ( CondNode a _ branches ) =
  CondNode a deps ( fixCondBranchConstraints <$> branches )
  where
  deps = Lens.view ( buildInfo' . targetBuildDepends' ) a

fixCondBranchConstraints
  :: ( HasBuildInfo a )
  => CondBranch v cs a
  -> CondBranch v [Dependency] a
fixCondBranchConstraints ( CondBranch cond true falseMay ) =
  CondBranch cond
    ( fixCondTreeConstraints true )
    ( fixCondTreeConstraints <$> falseMay )
