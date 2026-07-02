module Wasp.Util.Glob
  ( GlobPatterns,
    compileGlobPatterns,
    matchesAnyGlob,
    recursiveFileGlobsWithExtensions,
    dirAndDescendantsGlobs,
  )
where

import qualified System.FilePath as FP
import qualified System.FilePath.Glob as Glob

type GlobPatterns = [Glob.Pattern]

compileGlobPatterns :: [String] -> GlobPatterns
compileGlobPatterns = map Glob.compile

matchesAnyGlob :: GlobPatterns -> FilePath -> Bool
matchesAnyGlob patterns path = any (`Glob.match` path) patterns

recursiveFileGlobsWithExtensions :: FilePath -> [String] -> [String]
recursiveFileGlobsWithExtensions dir extensions =
  (\ext -> dir FP.</> "**" FP.</> "*" ++ ext) <$> extensions

dirAndDescendantsGlobs :: FilePath -> [String]
dirAndDescendantsGlobs dir =
  [ dir,
    dir FP.</> "**" FP.</> "*"
  ]
