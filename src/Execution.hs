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
-- Execution control.
--
-- This module contains an interface to the backend and functions to
-- control the execution of the compiler. 
--
---------------------------------------------------------------------------

module Execution (
                  abortCompiler,
                  stopCompiler,
                  compileC,
                  compileLLVM,
                  linkO,
                  linkBC
                 ) where
    
import System (system, exitWith, ExitCode(..))
import qualified Monad
import qualified Directory
import Common

-- Timber compiler
import Config
import Name

compile cfg clo file
    | doLLVM clo = compileLLVM cfg clo (file ++ ".ll")
    | otherwise  = fail $ show file -- compileC cfg clo (file ++ ".c")

link cfg clo files
    | doLLVM clo = linkBC cfg clo files
    | otherwise  = fail $ show files -- linkO cfg clo files


compileLLVM global_cfg clo ll_file = do
  let bc_file = rmSuffix ".ll" ll_file ++ ".bc"
      s_file  = rmSuffix ".ll" (rmDirs ll_file) ++ ".s"
      o_file  = rmSuffix ".ll" (rmDirs ll_file) ++ ".o"
  res <- checkUpToDate ll_file bc_file
  if not res then do
               cfg <- fileCfg clo ll_file global_cfg
               let cmdLLVMAS = llvmLLVMAS cfg ++ " -f " ++ ll_file
               putStrLn ("[compiling " ++ ll_file ++ "]")
               execCmd clo cmdLLVMAS
               let cmdLLC = llvmLLC cfg ++ bc_file ++ " -f -o " ++ s_file ++ " -march=x86"
               putStrLn ("[compiling " ++ bc_file ++ "]")
               execCmd clo cmdLLC
               let cmdAS = llvmAS cfg ++ s_file ++ " -o " ++ o_file
               putStrLn ("[compiling " ++ s_file ++ "]")
               execCmd clo cmdAS              
             else return ()
  where checkUpToDate ll_file bc_file = do 
          bc_exists <- Directory.doesFileExist bc_file
          if bc_exists then do
                        llvm_time <- Directory.getModificationTime ll_file
                        bc_time   <- Directory.getModificationTime bc_file
                        return (llvm_time <= bc_time)
                      else return False

{-
compileLLVM global_cfg clo ll_file = do
  let bc_file = rmSuffix ".ll" ll_file ++ ".bc"
      s_file  = rmSuffix ".ll" (rmDirs ll_file) ++ ".s"
      o_file  = rmSuffix ".ll" (rmDirs ll_file) ++ ".o"
  res <- checkUpToDate ll_file o_file
  if not res then do
               cfg <- fileCfg clo ll_file global_cfg
               let cmdLLVMAS = llvmLLVMAS cfg ++ " -f " ++ ll_file
               putStrLn ("[compiling " ++ ll_file ++ "]")
               execCmd clo cmdLLVMAS
               let cmdLLC = llvmLLC cfg ++ bc_file ++ " -f -o " ++ s_file ++ " -march=x86"
               putStrLn ("[compiling " ++ bc_file ++ "]")
               execCmd clo cmdLLC
               let cmdAS = llvmAS cfg ++ s_file ++ " -o " ++ o_file
               putStrLn ("[compiling " ++ s_file ++ "]")
               execCmd clo cmdAS              
             else return ()
  where checkUpToDate ll_file o_file = do 
          o_exists <- Directory.doesFileExist o_file
          if o_exists then do
                        llvm_time  <- Directory.getModificationTime ll_file
                        o_time     <- Directory.getModificationTime o_file
                        return (llvm_time <= o_time)
                      else return False
-}

-- | Compile a C-file. 
compileC global_cfg clo c_file = do 
  let bc_file = rmSuffix ".c" (rmDirs c_file) ++ ".o"
  res <- checkUpToDate c_file bc_file
  if not res then do
               putStrLn ("[compiling "++c_file++"]")
               cfg <- fileCfg clo c_file global_cfg
               let cmd = cCompiler cfg
                         ++ " -c " ++ compileFlags cfg
                         ++ " -I " ++ libDir clo ++ " " 
                         ++ " -I " ++ includeDir clo 
                         ++ " -I " ++ rtsDir clo ++ " " 
                         ++ " -I . "
                         ++ c_file
                         --return ()
               execCmd clo cmd
             else return ()
      where checkUpToDate c_file bc_file = do 
                                   bc_exists <- Directory.doesFileExist bc_file
                                   if not bc_exists then return False
                                                    else do
                                                      c_time  <- Directory.getModificationTime c_file
                                                      bc_time <- Directory.getModificationTime bc_file
                                                      return (c_time <= bc_time)

{-
-- | Compile a C-file. 
compileC global_cfg clo c_file
                        = do let o_file = rmSuffix ".c" (rmDirs c_file) ++ ".o"
                             res <- checkUpToDate c_file o_file
                             if not res then do
                                putStrLn ("[compiling "++c_file++"]")
                                cfg <- fileCfg clo c_file global_cfg
                                let cmd = cCompiler cfg
                                        ++ " -c " ++ compileFlags cfg
                                        ++ " -I " ++ libDir clo ++ " " 
                                        ++ " -I " ++ includeDir clo ++ " " 
                                        ++ " -I " ++ rtsDir clo ++ " " 
                                        ++ " -I . "
                                        ++ c_file
                                --return ()
                                execCmd clo cmd
                              else return ()
  where checkUpToDate c_file o_file
                        = do o_exists <- Directory.doesFileExist o_file
                             if not o_exists then
                                return False
                               else do
                                 c_time  <- Directory.getModificationTime c_file
                                 o_time  <- Directory.getModificationTime o_file
                                 return (c_time <= o_time)
-}

linkBC global_cfg clo r bc_files = do
  let bc_file = outfile clo ++ ".bc"
      o_file  = outfile clo ++ ".o"
      s_file  = outfile clo ++ ".s"
  putStrLn "[linking]"
  cfg <- foldr ((=<<) . fileCfg clo) (return global_cfg) bc_files
  let rootId     = name2str r
      Just rMod = fromMod r
      initId     = "_init_" ++ modToundSc rMod 
      -- link bc_files with libTimber.bc
      cmd1 = llvmLD cfg
             ++ rtsDir clo ++ "/libTimber.bc "
             ++ unwords bc_files
             ++ " -o " ++ bc_file
      -- apply llvm optimizations
      cmd2 = llvmOPT cfg
             ++ " -mem2reg -deadtypeelim "
             ++ llvmOptFlags clo ++ " "
             ++ bc_file
             ++ " -f -o " 
             ++ bc_file
      -- compile to native code
      cmd3 = llvmLLC cfg 
             ++ bc_file ++ 
             " -f -o " 
             ++ s_file 
             ++ " -march=x86"
      -- assemble native code
      cmd4 = llvmAS cfg 
             ++ s_file 
             ++ " -o " 
             ++ o_file
      -- compile main and link
      cmd5 = cCompiler cfg
             ++ " -pthread"
             ++ " -o " ++ outfile clo ++ " "
             ++ o_file
             ++ " -DROOT=" ++ rootId
             ++ " -DROOTINIT=" ++ initId ++ " "
             ++ rtsMain clo 
  tr $ cmd1
  tr $ cmd2
  tr $ cmd3
  tr $ cmd4
  tr $ cmd5
  execCmd clo cmd1
  execCmd clo cmd2
  execCmd clo cmd3
  execCmd clo cmd4
  execCmd clo cmd5

-- | Link together a bunch of object files.
linkO global_cfg clo r o_files =
                          do putStrLn "[linking]"
                             cfg <- foldr ((=<<) . fileCfg clo) (return global_cfg) o_files
                             let rootId     = name2str r
                                 Just rMod = fromMod r
                                 initId     = "_init_" ++ modToundSc rMod
                                 cmd = cCompiler cfg
                                       ++ linkFlags cfg
                                       ++ compileFlags cfg
                                       ++ " -o " ++ outfile clo ++ " "
                                       ++ unwords o_files ++ " "
                                       ++ " -L" ++ rtsDir clo ++ " " 
                                       ++ " -I" ++ includeDir clo ++ " " 
                                       ++ " -I" ++ libDir clo ++ " " 
                                       ++ " -I " ++ rtsDir clo ++ " " 
                                       ++ " -I . "  
                                       ++ " -DROOT=" ++ rootId ++ " "
                                       ++ " -DROOTINIT=" ++ initId ++ " "
                                       ++ rtsMain clo 
                                       ++ linkLibs cfg
                             tr $ cmd
--                             return ()
                             execCmd clo cmd

{-
-- | Link together a bunch of object files.
linkO global_cfg clo r o_files =
                          do putStrLn "[linking]"
                             cfg <- foldr ((=<<) . fileCfg clo) (return global_cfg) o_files
                             let rootId     = name2str r
                                 Just rMod = fromMod r
                                 initId     = "_init_" ++ modToundSc rMod
                                 cmd = cCompiler cfg
                                       ++ linkFlags cfg
                                       ++ compileFlags cfg
                                       ++ " -o " ++ outfile clo ++ " "
                                       ++ unwords o_files ++ " "
                                       ++ " -L" ++ rtsDir clo ++ " " 
                                       ++ " -I" ++ includeDir clo ++ " " 
                                       ++ " -I" ++ libDir clo ++ " " 
                                       ++ " -I " ++ rtsDir clo ++ " " 
                                       ++ " -I . "  
                                       ++ " -DROOT=" ++ rootId ++ " "
                                       ++ " -DROOTINIT=" ++ initId ++ " "
                                       ++ rtsMain clo 
                                       ++ linkLibs cfg
                             tr $ cmd
--                             return ()
                             execCmd clo cmd
-}

-- | Return with exit code /= 0
abortCompiler         = exitWith (ExitFailure 1)

-- | Return with exit code == 0
stopCompiler          = exitWith (ExitSuccess)

execCmd clo cmd       = do Monad.when (isVerbose clo)
                                    (putStrLn ("exec: " ++ show cmd))
                           exitCode <- system $ cmd     
                           case exitCode of
                             ExitSuccess -> return ()
                             _           -> stopCompiler


fileCfg clo file global_cfg =
    do b <- Directory.doesFileExist file_cfg
       if b
         then do Monad.when (isVerbose clo) (putStrLn ("using configuration file: " ++ file_cfg))
                 augmentCfg `fmap` parseCfg file_cfg
         else return global_cfg
  where
    file_cfg = base file ++ ".extern."++target clo++".cfg"

    augmentCfg cfg =
        global_cfg { compileFlags = join compileFlags,
                     linkFlags    = join linkFlags,
                     linkLibs     = join linkLibs }
      where
        join f = f cfg +++ f global_cfg

    -- Assume each flag is a word.
    -- Combine sequences of flags by joining them and removing duplicates.
    fs1 +++ fs2  = ' ':unwords (nub (words fs1 ++ words fs2))

    base path = case break (`elem` "/.") (reverse path) of
                  (rext,'.':rbase) -> reverse rbase
                  _ -> path
