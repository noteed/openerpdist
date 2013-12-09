{-# LANGUAGE RecordWildCards #-}
-- | Read an __openerp__.py file into a data structure meant to represent it.
module OpenERP.Dist.Descriptor where

import Control.Monad (filterM)
import Data.Char
import Data.List (nub)
import Data.Monoid
import Language.Python.Common.AST
import Language.Python.Version2.Parser (parseModule)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Exit (exitFailure)
import System.FilePath ((</>), splitDirectories, takeExtension)

-- | Generate a `setup.py` file for a module given its path.
generateSetup :: Maybe String -> String -> IO ()
generateSetup mversion modulePath = do
  -- TODO check directory exists.
  -- TODO check file exists.
  let filename = modulePath </> "__openerp__.py"
      moduleName = last $ splitDirectories modulePath
  content <- readFile filename
  let content' = hackChars content ++ "\n"
  case parseModule content' filename of
    Right (Module [StmtExpr (Dictionary d _) _], _) -> do
      subs <- findSubPackages moduleName modulePath
      dataFiles <- findData "."
        [ ".ico", ".gif", ".jpg", ".jpeg", ".png"
        , ".eot", ".svg", ".ttf", ".woff" -- webfonts, not sure if they are all needed.
        , ".js" -- some are referenced in the __openerp__.py file, some aren't.
        , ".csv", ".xml", ".yml", ".eml", ".html", ".mako" -- html for report_webkit
        , ".rml", ".sxw", ".xsl"
        , ".po", "*.pot"
        ]
        modulePath
      writeFile (modulePath </> "setup.py") $ pythonSetup $
        descriptorToSetup mversion moduleName subs dataFiles $
        foldl mappend emptyDescriptor $ map entryToDescriptor d
      let f [_] = ""
          f (x:xs) = x
          f [] = ""
          dirs = nub $ map (f . splitDirectories) dataFiles
      writeFile (modulePath </> "MANIFEST.in") $ unlines $
        [ "graft doc"
        , "prune doc/_build"
        , "include README"
        , "include LICENSE"
        , "include MANIFEST.in"
        ] ++ concatMap (\d ->
          -- TODO we should write only the patterns that make sense for this
          -- particular addons
          [ (if d == "" then "include" else "recursive-include " ++ d) ++ " *.csv *.xml *.yml *.eml *.html *.mako *.ico *.gif *.jpg *.jpeg *.png *sxw *.xsl"
          , (if d == "" then "include" else "recursive-include " ++ d) ++ " *.eot *.svg *.ttf *.woff *.js *.rng *.sql *.rml *.po *.pot"
          ]) dirs ++
        [ "global-exclude *pyc *~"
        ]
    Right _ -> do
      putStrLn "The module must contain a single dictionary."
      exitFailure
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure

findSubPackages :: FilePath -> FilePath -> IO [FilePath]
findSubPackages name modulePath = do
  dirs_ <- getDirectoryContents modulePath
  let dirs = filter (\d -> d /= "." && d /= "..") dirs_
  dirs' <- filterM (doesFileExist . (\d -> modulePath </> d </> "__init__.py")) dirs
  subdirs <- mapM (\d -> findSubPackages d (modulePath </> d)) dirs'
  return $ map (\d -> name ++ "." ++ d) $ dirs' ++ concat subdirs

findData :: FilePath -> [String] -> FilePath -> IO [FilePath]
findData name exts modulePath = do
  content_ <- getDirectoryContents modulePath
  let content = filter (\d -> d /= "." && d /= "..") content_
  dirs <- filterM (doesDirectoryExist . (modulePath </>)) content
  files <- filterM (doesFileExist . (modulePath </>)) content
  subfiles <- mapM (\d -> findData d exts (modulePath </> d)) dirs
  let files' = filter (matchExtensions exts) files
  return $ map (\d -> if name == "." then d else name ++ "/" ++ d) $ files' ++ concat subfiles

matchExtensions :: [String] -> FilePath -> Bool
matchExtensions exts filename = takeExtension filename `elem` exts

-- | Represent a complete __openerp__.py file.
data Descriptor = Descriptor
  { descriptorName :: String
  , descriptorVersion :: String
  , descriptorCategory :: String
  , descriptorDescription :: String
  , descriptorAuthor :: String
  , descriptorWebsite :: String
  , descriptorInstallable :: Maybe Bool
  , descriptorAutoInstall :: Maybe Bool
  , descriptorDependencies :: [String]
  , descriptorData :: [String]
  , descriptorDemo :: [String]
  , descriptorQWeb :: [String]
  , descriptorCss :: [String]
  , descriptorJs :: [String]
  }
  deriving Show

instance Monoid Descriptor where
  mempty = emptyDescriptor
  mappend a b = Descriptor
    (k (descriptorName a) (descriptorName b))
    (v (descriptorVersion a) (descriptorVersion b))
    (k (descriptorCategory a) (descriptorCategory b))
    (k (descriptorDescription a) (descriptorDescription b))
    (k (descriptorAuthor a) (descriptorAuthor b))
    (k (descriptorWebsite a) (descriptorWebsite b))
    (m (descriptorInstallable a) (descriptorInstallable b))
    (m (descriptorAutoInstall a) (descriptorAutoInstall b))
    (descriptorDependencies a `mappend` descriptorDependencies b)
    (descriptorData a `mappend` descriptorData b)
    (descriptorDemo a `mappend` descriptorDemo b)
    (descriptorQWeb a `mappend` descriptorQWeb b)
    (descriptorCss a `mappend` descriptorCss b)
    (descriptorJs a `mappend` descriptorJs b)
    where k c d = if null d then c else d
          v c d = if d == "0.0" then c else d
          m c d = if d == Nothing then c else d

-- | The default (empty) descriptor.
emptyDescriptor :: Descriptor
emptyDescriptor = Descriptor "" "0.0" "" "" "" "" Nothing Nothing [] [] [] [] [] []

-- | Process an __openerp__.py entry into a partial descriptor.
-- `mappend`ing all the partial descriptors should result in a complete
-- descriptor (i.e. corresponding to the whole __openerp__.py).
entryToDescriptor :: (ExprSpan, ExprSpan) -> Descriptor
entryToDescriptor e = case e of
  (Strings s _, Strings x _) -> case concatStrings s of
    "name"-> emptyDescriptor { descriptorName = concatStrings x }
    "version"-> emptyDescriptor { descriptorVersion = concatStrings x }
    "category"-> emptyDescriptor { descriptorCategory = concatStrings x }
    "description"-> emptyDescriptor { descriptorDescription = concatStrings x }
    "author"-> emptyDescriptor { descriptorAuthor = concatStrings x }
    "website"-> emptyDescriptor { descriptorWebsite = concatStrings x }
    _ -> emptyDescriptor
  -- TODO Urk, duplicated code.
  (Strings s _, x) -> case concatStrings s of
    "installable" -> case x of
      Var (Ident "True" _) _ ->
        emptyDescriptor { descriptorInstallable = Just True }
      Var (Ident "False" _) _ ->
        emptyDescriptor { descriptorInstallable = Just False }
      _ -> error $ "installable must be True or False but it is " ++ show x
    "auto_install" -> case x of
      Var (Ident "True" _) _ ->
        emptyDescriptor { descriptorAutoInstall = Just True }
      Var (Ident "False" _) _ ->
        emptyDescriptor { descriptorAutoInstall = Just False }
      _ -> error $ "auto_install must be True or False but it is " ++ show x
    "depends" -> case x of
      List l _ ->
        emptyDescriptor { descriptorDependencies = map concatStrings' l }
      _ -> error $ "depends must be a list of strings but it is " ++ show x
    "data" -> case x of
      List l _ -> emptyDescriptor { descriptorData = map concatStrings' l }
      _ -> error $ "data must be a list of strings but it is " ++ show x
    "demo" -> case x of
      List l _ -> emptyDescriptor { descriptorDemo = map concatStrings' l }
      _ -> error $ "demo must be a list of strings but it is " ++ show x
    "qweb" -> case x of
      List l _ -> emptyDescriptor { descriptorQWeb = map concatStrings' l }
      _ -> error $ "qweb must be a list of strings but it is " ++ show x
    "css" -> case x of
      List l _ -> emptyDescriptor { descriptorCss = map concatStrings' l }
      _ -> error $ "css must be a list of strings but it is " ++ show x
    "js" -> case x of
      List l _ -> emptyDescriptor { descriptorJs = map concatStrings' l }
      _ -> error $ "js must be a list of strings but it is " ++ show x
  -- TODO Handle everything, e.g. images is missing.
    _ -> emptyDescriptor
  _ -> emptyDescriptor

-- | "Evaluate" juxtaposed string literals to a single string value.
concatStrings' :: ExprSpan -> String
concatStrings' (Strings s _) = concatStrings s
concatStrings' x = error $
  "concatStrings': unexpected ExprSpan constructor (expected Strings): "
  ++ show x

-- | "Evaluate" juxtaposed string literals to a single string value.
concatStrings :: [String] -> String
concatStrings = unhackChars . concat . map stripQuote

-- | Remove the single/triple quotes of Python string literals to create a
-- string value.
stripQuote :: String -> String
stripQuote ('"':'"':'"':rest) = take (length rest - 3) rest
stripQuote ('\'':'\'':'\'':rest) = take (length rest - 3) rest
stripQuote ('"':rest) = init rest
stripQuote ('\'':rest) = init rest

-- Unicode hack. It seems Alex (used by language-python) does not have support
-- for UTF-8. So we change back and forth non-ascii character to {{{decimal}}}
-- where decimal is the 'ord' of the character.

hackChars = concatMap hackChar

hackChar c | ord c > 127 = "{{{" ++ show (ord c) ++ "}}}"
hackChar c = [c]

unhackChars [] = []
unhackChars ('{':'{':'{':rest) = chr (read n) : unhackChars rest'
  where n = takeWhile (`elem` ['0'..'9']) rest
        rest' = drop (length n + 3) rest
unhackChars (c:cs) = c : unhackChars cs

shebang = "#! /usr/bin/env python2"

tripleQuotes = ("\"\"\"" :) . (++ ["\"\"\""])
tripleQuotes' = ("\"\"\"" :) . (++ ["\"\"\","])

-- | Create a docstring for a `setup.py` given a module name.
docstring :: String -> String
docstring name = unlines $ tripleQuotes $
  [ "Replacement (unofficial) `setup.py` for the `" ++ name ++ "` module."
  , "Automatically generated by `openerpdist`. See \
    \http://noteed.com/openerpdist/."
  ]

-- | Import statement for a setuptools-based `setup.py`.
imports :: String
imports = "import setuptools"

-- | Represent a call to setuptools.setup(). Only the fields we care of are
-- represented.
data Setup = Setup
  { setupName :: String
  , setupVersion :: String
  , setupDescription :: String
  , setupLongDescription :: String
  , setupUrl :: String
  , setupAuthor :: String
  , setupAuthorEmail :: String
  , setupClassifiers :: [String]
  , setupLicense :: String
  , setupPackageDir :: [(String, String)]
  , setupPackages :: [String]
  , setupPackageData :: [(String, [String])]
  , setupInstallRequires :: [String]
  , setupTestsRequire :: [String]
  }

-- | Convert a `Setup` object to a string (intended to be the content of a
-- `setup.py` file).
pythonSetup :: Setup -> String
pythonSetup s = unlines $
  [ shebang
  , "# -*- coding: utf-8 -*-"
  , docstring (setupName s)
  , imports
  , ""
  ] ++ callSetup s

-- | Helper used in `pythonSetup`.
callSetup :: Setup -> [String]
callSetup Setup{..} =
  [ "setuptools.setup("
  , "name" .= openerpaddons setupName
  , "version" .= setupVersion
  , "description" .= setupDescription
  , "    long_description = "
  ] ++ tripleQuotes' (lines setupLongDescription) ++
  [ "url" .= setupUrl
  , "author" .= setupAuthor
  , "author_email" .= setupAuthorEmail
  , "classifiers" `listOfStrings` setupClassifiers
  , "license" .= setupLicense
  , "package_dir" `dictOfStrings` setupPackageDir
  , "packages" .= setupPackages
  , "package_data" `dictOfLists` setupPackageData
  , "install_requires" .= setupInstallRequires
  , "tests_require" .= setupTestsRequire
  , ")"
  ]
  where a .= b = "    " ++ a ++ " = " ++ show b ++ ","
        a ~= b = "    '" ++ a ++ "': " ++ show b ++ ","
        a `listOfStrings` b = "    " ++ a ++ " = [" ++ concatMap ((++ ",") . ("\n        " ++) . show) b ++ "\n    ],"
        a `dictOfLists` xs = "    " ++ a ++ " = {\n" ++ concatMap (("    " ++) . uncurry (~=)) xs ++ "\n    },"
        a `dictOfStrings` xs = "    " ++ a ++ " = {\n" ++ concatMap (("    " ++) . uncurry (~=)) xs ++ "\n    },"

-- | Convert an OpenERP technical addons name to a package name, e.g. `sale`
-- becomes `openerp-sale`.
openerpaddons :: String -> String
openerpaddons name =
  "openerp-" ++ map (\c -> if c == '_' then '-' else c) name

-- | Convert a Descriptor value to a Setup value.
descriptorToSetup :: Maybe String -> String -> [String] -> [String] -> Descriptor -> Setup
descriptorToSetup mversion name subs dataFiles Descriptor{..} = Setup
  { setupName = name
  , setupVersion = maybe "7.0.1" id mversion
  , setupDescription = descriptorName
  , setupLongDescription = descriptorDescription
  , setupUrl = descriptorWebsite
  , setupAuthor = descriptorAuthor
  , setupAuthorEmail = "TODO"
  , setupClassifiers =
    [ "Development Status :: 5 - Production/Stable"
    , "Environment :: Plugins"
   -- TODO
   -- "Framework :: OpenERP"
    , "Intended Audience :: Developers"
    , "License :: OSI Approved :: GNU Affero General Public License v3"
    , "Natural Language :: English"
    , "Programming Language :: Python :: 2.7"
    , "Programming Language :: Python :: Implementation :: CPython"
    ]
  , setupLicense = "AGPL-3"
  , setupPackageDir = [("openerp.addons." ++ name, ".")]
  , setupPackages = map ("openerp.addons." ++) $ name : subs
  , setupPackageData = [("openerp.addons." ++ name, ["i18n/*.po*"]
    ++ dataFiles
    ++ packageData descriptorData descriptorDemo descriptorQWeb descriptorCss descriptorJs)]
  , setupInstallRequires = nub $ map convertDependency descriptorDependencies
    ++ hardcodedRequires name
  , setupTestsRequire = ["unittest2"]
  }

-- | Convert a dependency as listed in the descriptor to a package name, e.g.
-- `sale` becomes `openerp-sale` and `base` becomes `openerp-core` (i.e. a
-- dependency on the `base` module becomes a dependency on the core library
-- package).
convertDependency :: String -> String
convertDependency "base" = "openerp-core"
convertDependency name = openerpaddons name

-- | Provide dependencies that are not listed in the __openerp__.py of some
-- addons (but that are likely listed in the server's __openerp__.py).
hardcodedRequires :: String -> [String]
hardcodedRequires "email_template" = ["Jinja2"]
hardcodedRequires "auth_ldap" = ["python-ldap"]
hardcodedRequires "auth_openid" = ["python-openid"]
hardcodedRequires "document_webdav" = ["PyWebDAV"]
hardcodedRequires "report_webkit" = ["Jinja2"]
hardcodedRequires "web" = ["mock"]
-- delivery depends on sale and stock but not explicitely on sale_stock.
-- This might qualify as a bug in delivery's __openerp__.py ?
-- Bug filled: https://bugs.launchpad.net/openobject-addons/+bug/1258973.
hardcodedRequires "delivery" = ["openerp-sale-stock"]
hardcodedRequires "project_mrp" = ["openerp-sale-stock"]
hardcodedRequires "stock_invoice_directly" = ["openerp-sale-stock"]
hardcodedRequires _ = []

packageData dat demo qweb css js = concat [dat, demo, qweb, css, js]
