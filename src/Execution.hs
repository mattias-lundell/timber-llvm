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

compileLLVM global_cfg clo ll_file = do
  let bc_file = rmSuffix ".ll" ll_file ++ ".bc"
  res <- checkUpToDate ll_file bc_file
  if not res then do
               cfg <- fileCfg clo ll_file global_cfg
               let cmdLLVMAS = llvmLLVMAS cfg ++ " -f " ++ ll_file
                   cmdLLVMOPT = llvmOPT cfg
                                ++ " -mem2reg "
                                ++ " -std-compile-opts "
                                ++ " -std-link-opts "
                                ++ " -strip "
                                ++ bc_file
                                ++ " -f -o " ++ bc_file
               putStrLn ("[compiling " ++ ll_file ++ "]")
               execCmd clo cmdLLVMAS
               execCmd clo cmdLLVMOPT
             else return ()
  where checkUpToDate ll_file bc_file = return False --do
-- XXX change this, all files are recomiled each time to prevent name clashes when linking
--          bc_exists <- Directory.doesFileExist bc_file
--          if bc_exists then do llvm_time <- Directory.getModificationTime ll_file
--                               bc_time   <- Directory.getModificationTime bc_file
--                               return (llvm_time <= bc_time)
--                       else return False

-- | Compile a C-file.
compileC global_cfg clo c_file = do
  let o_file = rmSuffix ".c" (rmDirs c_file) ++ ".o"
  res <- checkUpToDate c_file o_file
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
               execCmd clo cmd
             else return ()
      where checkUpToDate c_file o_file = do
                                   o_exists <- Directory.doesFileExist o_file
                                   if not o_exists then return False
                                                    else do
                                                      c_time <- Directory.getModificationTime c_file
                                                      o_time <- Directory.getModificationTime o_file
                                                      return (c_time <= o_time)

linkBC global_cfg clo r bc_files = do
  let bc_file    = outfile clo ++ ".bc"
      o_file     = outfile clo ++ ".o"
      s_file     = outfile clo ++ ".s"
      tmp_bcfile = outfile clo ++ "tmp.bc"
  putStrLn "[linking]"
  cfg <- foldr ((=<<) . fileCfg clo) (return global_cfg) bc_files
  let rootId     = name2str r
      Just rMod = fromMod r
      initId     = "_init_" ++ modToundSc rMod
      -- create main.bc
      cmd1 = llvmLLVMAS cfg
             ++ " main.ll "
             ++ " -o main.bc"
      -- link main and program files
      cmd2 = llvmLD cfg
             ++ " -Xlinker='-m32' "
             ++ " -native "
             ++ " -internalize "
             ++ " -L" ++ rtsDir clo
             ++ " -o " ++ outfile clo ++ " "
             ++ " main.bc " ++ unwords bc_files ++ " "
             ++ " -b " ++ tmp_bcfile ++ " "
             ++ " -lTimberLLVMLIB "
             ++ " -lTimberLLVMRTS "
             ++ " -lpthread"
  writeFile "main.ll" (llvmMain rMod)
  execCmd clo cmd1
  execCmd clo cmd2

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
                             execCmd clo cmd

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

--   \%PTHREAD_MUTEX_T = type {[44 x i8]}\n\
llvmMain mod =
  "%Ref = type {i32*, %PTHREAD_MUTEX_T, i32*}\n\
  \%PTHREAD_MUTEX_T = type {[24 x i8]}\n\
  \%WORLD = type {opaque}\n\
  \declare void @_init_"++ mod ++ "()\n\
  \declare void @root_" ++ mod ++ "(%WORLD*, %Ref*)\n\
  \declare %Ref* @init_rts(i32, i8**)\n\
  \declare void @pruneStaticHeap()\n\
  \declare i32 @sleep_rts()\n\n\
  \define i32 @main(i32 %argc, i8** %argv) nounwind {\n\
  \  entry:\n\
  \  %envObj =tail call %Ref* @init_rts(i32 %argc, i8** %argv) nounwind\n\
  \  tail call void @_init_" ++ mod ++ "() nounwind\n\
  \  tail call void @pruneStaticHeap() nounwind\n\
  \  tail call void @root_" ++ mod ++ "(%WORLD* null, %Ref* %envObj) nounwind\n\
  \  tail call i32 @sleep_rts() nounwind\n\
  \  ret i32 0\n\
  \}\n"