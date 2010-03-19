-- The Timber compiler <timber-lang.org>
--
-- Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the names of the copyright holder and any identified
--    contributors, nor the names of their affiliations, may be used to 
--    endorse or promote products derived from this software without 
--    specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

--------------------------------------------------------------------------
--
-- Compiler configuration
--
-- This module contains both what GHC calls "dynamic flags", flags that
-- can change between each compilation, and static flags. Initial idea
-- from GHC, slightly modified to fit our needs.
--
---------------------------------------------------------------------------

module Config (
               -- Our Data from files/flags.
               CfgOpts(..),
               CmdLineOpts(..),
               
               -- Read out configuration from file.
               readCfg,
               parseCfg,

               -- Query about our options from flags.
               cmdLineOpts,
                          
               -- Configurable paths
               libDir,
               includeDir,
               rtsDir,
               rtsCfg,
               rtsMain,
               
               -- Print usage info and exit.
               helpMsg,
                      
               -- Pass data. XXX Does not belong here?
               Pass(..),
               allPasses,
                        
               -- Dynamic Exceptions
               TimbercException(..),
               ) where


import Char
import System.Console.GetOpt
import System                 ( getArgs, getEnv, getProgName )
import qualified Control.Exception as Exception
import Data.Dynamic


versionString = "The Timber compiler, version 1.0.3"

-- | Contains the configuration for the compiler with the current
-- command line switches.
data CfgOpts         = CfgOpts { cCompiler       :: FilePath,
                                 llvmLLVMAS      :: FilePath,
                                 llvmLLC         :: FilePath,
                                 llvmAS          :: FilePath,
                                 llvmLD          :: FilePath,
                                 llvmOPT         :: FilePath,
                                 compileFlags    :: String,
                                 linkFlags       :: String,
                                 linkLibs        :: String
                               } deriving (Show, Eq, Read)

-- | Command line options.
data CmdLineOpts     = CmdLineOpts { isVerbose    :: Bool,
                                     datadir      :: FilePath,
                                     target       :: String,
                                     outfile      :: FilePath,
                                     root         :: String,
                                     make         :: String,
                                     api          :: Bool,
                                     pager        :: FilePath,
                                     shortcut     :: Bool,
                                     stopAtC      :: Bool,
                                     stopAtO      :: Bool,
                                     doScp        :: Bool,
                                     dumpAfter    :: Pass -> Bool,
                                     stopAfter    :: Pass -> Bool,
                                     doLLVM       :: Bool,
                                     llvmOptFlags :: String   
                                   }

options              :: [OptDescr Flag]
options              = [ Option []
                                ["help"]
                                (NoArg Help)
                                "Show this message",
                         Option [] 
                                ["version"] 
                                (NoArg Version)
                                "Print version information",
                         Option [] 
                                ["print-datadir"] 
                                (NoArg PrintDatadir)
                                "Print effective datadir path",
                         Option ['v'] 
                                ["verbose"] 
                                (NoArg Verbose)
                                "Be verbose",
                         Option []
                                ["datadir"]
                                (ReqArg Datadir "DIRECTORY")
                                "Path to installed libraries and run-time systems",
                         Option []
                                ["target"]
                                (ReqArg Target "TARGET")
                                "Target platform",
                         Option ['o'] 
                                ["output"]  
                                (ReqArg Outfile "FILE")     
                                "Name of executable output",
                         Option []
                                ["root"]
                                (ReqArg Root "[MODULE.]NAME")
                                "Define root of executable program",
                         Option []
                                ["make"]
                                (ReqArg Make "[MODULE.]NAME")
                                "Make program with given root module",
                         Option []
                                ["api"]
                                (NoArg Api)
                                "Produce html file with API",
                         Option [] 
                                ["pager"] 
                                (ReqArg Pager "PATH")
                                "Use command PATH to display .html files",
                         Option ['s']
                                ["shortcut"]
                                (NoArg ShortCut)
                                "Try to reuse existing .c, .h and .ti files",
                         Option ['C'] 
                                ["stop-at-c"]
                                (NoArg StopAtC)
                                "Stop compiler after .c file generation",
                         Option ['c'] 
                                ["stop-at-o"] 
                                (NoArg StopAtO)
                                "Stop compiler after .o file generation",
                         Option ['S']
                                ["scp"]
                                (NoArg DoScp)
                                "Supercompile the program",
                         Option []
                                ["llvm"]
                                (NoArg LLVM)
                                "Compile to LLVM",
                         Option []
                                ["llvmopt"]
                                (ReqArg LLVMOptFlags "LLVM-flag")
                                "Provide LLVM optimization flags"
                       ]
                       ++ 
                       [ Option []
                                ["dump-" ++ map Char.toLower (show pass)]
                                (NoArg $ DumpAfter pass)
                                ("Dump " ++ show pass ++ " output to stdout")
                       | pass <- allPasses 
                       ] 
                       ++ 
                       [ Option []
                                ["stop-after-" ++ map Char.toLower (show pass)]
                                (NoArg $ StopAfter pass)
                                ("Stop compiler after pass " ++ show pass)
                       | pass <- allPasses 
                       ]
                       



data Flag            = Help
                     | Verbose
                     | Version
                     | PrintDatadir
                     | Datadir String
                     | Target String
                     | Outfile String
                     | Root String
                     | Make String
                     | Api
                     | Pager String
                     | ShortCut
                     | StopAtC
                     | StopAtO
                     | DoScp
                     | DumpAfter Pass
                     | StopAfter Pass
                     | LLVM
                     | LLVMOptFlags String
                     deriving (Show,Eq)

data TimbercException
  = CmdLineError String     -- User did something wrong with command line options.
  | ConfigError String      -- Something is wrong with the given compiler configuration.
  | CompileError String     -- Error in the given Timber input file.
  | Panic String            -- Unexpected faults in timberc, panic.
  deriving (Eq)

instance Typeable TimbercException where
  typeOf _ = mkTyConApp (mkTyCon "TimbercException") []

-- | Let us show our exceptions as expected.
instance Show TimbercException where
  showsPrec _ (CmdLineError s)      = showString s
  showsPrec _ (ConfigError s)       = showString s
  showsPrec _ (CompileError s)      = showString s
  showsPrec _ (Panic s)             = showString ("**** Internal compiler panic! [Please file a bug report...]\n\n" ++ s)

instance Exception.Exception TimbercException

cmdLineOpts          :: [String] -> IO (CmdLineOpts,[String])
cmdLineOpts args     = do pager <- System.getEnv "PAGER" `catch` (\_ -> return "")
                          let pagerOpt = if null pager then [] else ["--pager", pager]
                          home <- System.getEnv "HOME" `catch` (\_ -> return "")
                          cfgFile <- System.getEnv "TIMBERC_CFG" `catch` (\_ -> return (home ++ "/.timberc"))
                          cfgOpts <- readFile cfgFile `catch` (\_ -> return "")
                          case getOpt Permute options (args ++ words cfgOpts ++ pagerOpt) of
                            (flags,n,[]) 
                              | Help `elem` flags -> do 
                                  msg <- helpMsg
                                  Exception.throwIO (CmdLineError msg)
                              | Version `elem` flags ->
                                  Exception.throwIO (CmdLineError versionString)
                              | PrintDatadir `elem` flags ->
                                  Exception.throwIO (CmdLineError (datadir (mkCmdLineOpts flags)))
                              | otherwise -> 
                                  return (mkCmdLineOpts flags, n)
                            (_,_,errs) -> do 
                                  msg <- helpMsg
                                  Exception.throwIO (CmdLineError (concat errs ++ msg))


helpMsg              = do pgm <- getProgName
                          return (usageInfo (header pgm) options)
  where header pgm   = "Usage: " ++ pgm ++ " [OPTION...] files..."
                         


-- CmdLineOpts is a set of functions that can be queried
-- about the command line options.

mkCmdLineOpts :: [Flag] -> CmdLineOpts
mkCmdLineOpts flags =  
    CmdLineOpts { isVerbose    = find Verbose,
                  datadir      = first ""
                                 [ dir | (Datadir dir) <- flags ],
                  target       = first "POSIX"
                                 [ target | (Target target) <- flags ],
                  outfile      = first "a.out"
                                 [ file | (Outfile file) <- flags ],
                  root         = first "root"
                                 [ root | (Root root) <- flags ],
                  make         = first ""
                                 [ root | Make root <- flags ],
                  api          = find Api,
                  pager        = first "less"
                                 [ p | (Pager p) <- flags ],
                  shortcut     = find ShortCut,
                  stopAtC      = find StopAtC,
                  stopAtO      = find StopAtO,
                  doScp        = find DoScp,
                  dumpAfter    = find . DumpAfter,
                  stopAfter    = find . StopAfter,
                  doLLVM       = find LLVM,
                  llvmOptFlags = first ""
                                 [ llvmflags | LLVMOptFlags llvmflags <- flags ]
                }
  where 
    first def []      = def
    first def (opt:_) = opt
    find  opt         = first False [ True | flag <- flags, flag == opt ]

-- | Read the configuration file from the standard locaton for the target.
readCfg clo = parseCfg (rtsCfg clo)

-- | Read the configuration at the given location.
parseCfg file
    = do
      txt <- catch (readFile file)
             (\e -> Exception.throwIO $ emsg file)
      config <- safeRead file txt
      return config
    where        
        safeRead :: FilePath -> String -> IO CfgOpts
        safeRead file text =
            let val = case [x | (x,t) <- reads text, ("","") <- lex t] of
                      [x] -> x
                      [] -> Exception.throw (ConfigError ("Parse error in " ++ file ++ "\n"))
                      _ -> Exception.throw (ConfigError ("File not found: " ++ file ++ "\n"))
                in (return $! val)
        emsg f = (CmdLineError $ "Could not open " ++ file)


-- | The different compiler passes.
-- XXX Should not be in this file.
data Pass            = Parser
                     | Desugar1
                     | Rename
                     | Desugar2
                     | S2C
                     | KCheck
                     | TCheck
                     | Termred
                     | SCP
                     | Type2
                     | C2K
                     | Kindlered
                     | LLift
                     | Prepare4C
                     | K2C
                     | K2LLVM
                     | Main                  -- top level 'pass'
                     deriving (Show,Eq,Ord,Enum)
                        
allPasses            :: [Pass]
allPasses            = [Parser .. K2C]


rtsDir clo           = datadir clo ++ "/rts" ++ target clo
rtsCfg clo           = rtsDir clo ++ "/timberc.cfg"
rtsMain clo          = rtsDir clo ++ "/main.c"
libDir clo           = datadir clo ++ "/lib"
includeDir clo       = datadir clo ++ "/include"
