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

module Main where

-- Haskell98
import System(getArgs)
import List(isSuffixOf)
import qualified Monad
import qualified Char
import qualified Control.Exception as Exception ( catch, catchJust )
import System.Console.GetOpt
import qualified Directory

-- Timber Compiler
import System.FilePath
import Config
import Execution
import PP
import Common
import Parser
import qualified Syntax
import qualified Core
import Desugar1
import Rename
import Desugar2
import Depend
import Syntax2Core
import Kind
import Type
import Termred
import Type2
import Kindle
import Kindlered
import Prepare4C
import Kindle2C
import Core2Kindle
import Lambdalift
import Data.Binary
import Interfaces
import GHC.Exception
import Scp
{-

Lexer:
- Checks lexical syntax and builds token stream
- Removes comments

Parser:
- Checks basic syntax and builds parse tree
- Builds constructor syntax (parsed as qualified types)
- Resolves nested infix expressions
- Converts record labels to proper selectors (with prefix .)
- Replaces prefix tuple constructors with primitive names

[A] = static checks
[B] = light-weight transformation (easily reversible, need to be done before type checking)
[C] = heavy-weight transformation (delay until after all static checking, including type checking)
[D] = unwanted transformation, propagate original form to the back-end

Desugar1:
- [A]  Checks record type acyclicity and selector consistence
- [B?] Completes dotdot record patterns and expressions with missing fields
- [B?] Translates instance declarations into instance signatures and named record terms
- [B?] Checks and resolves if/elsif/else statements
- [B?] Makes the "self" variable explicit.
- [A]  Checks validity of result statement
- [C?] Replaces prefix expression statement with dummy generator statement
- [D]  Replaces if/case statements with corresponding expressions forms
- [A]  Checks the restricted command syntax of template bodies

Rename:
- [A] Checks for duplicate declarations and kind signatures
- [A] Checks for duplicate selectors and constructors, and overlaps with class members
- [A] Checks for duplicate equations and type signatures
- [A] Checks for duplicated variables in patterns and templates
- [A] Checks for duplication and dangling abstractions in type predicates
- [A] Checks for unbound variables (not yet implemented -- what to do with globals?)

- [B] Makes every type variable binding explicit in signatures and constructor declarations
- [B] Shuffles signatures as close as possible to the corresponding definition (incl. inside patterns)

- [B] Renames local variables using unique names
- [B] Renames local (explicitly quantified) type variables using unique names

Desugar2:
- [A] Checks validity of type and type scheme syntax in signatures and declarations
- [A] Checks qualified types for syntactic ambiguity
- [B] Removes special syntax for list, tuple and function types, as well as subtype predicates
- [B] Captures single-variable predicates as short-hand for wildcard kind signature
- [A] Checks syntactic validity of patterns
- [B] Replaces after, before, [] and (:) with primitive names
- [B] Replaces tuple expressions with primitive constructor applications 
- [B] Translates if expressions, operator sections, negations
- [A][B] Collects functions defined with multiple equations while checking arities
- [C] Flattens pattern bindings
- [C] Removes non-trivial patterns in lambdas, assignments and generators
- [C] Translates list expressions/patterns and list comprehensions

- [C] Applies pattern-matching compiler to case expressions

Syntax2Core:
- Injects instance signatures among global type signatures

- Splits predicate contexts into kind environments (quantifiers) and predicates proper (qualifiers)
- Checks validity of base type syntax in declarations
- Replaces _ kinds with unique unification variable
- Replaces _ types with unique unification variable

- Applies up/down signature propagation within constructor/selector/member/signature environment
- Completes bindings and lambdas with missing signatures

- Does dependency analysis on local bindings
- Builds Core syntax tree

Kind:
- Does dependency analysis on type declarations
- Performs kind inference, replacing every kind unification variable (defaulting to *)

Decls:
- Extracts selector and constructor type schemes
- Generates global member signatures and bindings
- Replaces subtyping in type declarations with explicit coercion constructors/selectors

- Initiate subtyping graphs with reflexivity axioms (identity witness)
- Computes subtype above & below graph, closed under transitivity (checks cyclic and ambiguity errors)
- Computes instance overlap graph, closed under superclass relation (checks cyclic and ambiguity errors)

Type:
- Does dependency analysis on top-level bindings
- Performs type inference and constraint simplification/solving
- Makes all function calls match the arity of the called function
- Inserts explicit overloading and subtyping witnesses

Termred:
- Does simple term level reduction and restricted inlining within primitive environment (witnesses only)

Core2Kindle:
- Converts type expressions by making closure types explicit
- Approximates polymorphism by the wildcard type
- Splits non-trivial datatypes into constructor records with a common tag field

- Encodes monadic code by means of extra (mutable) state argument

- Sorts equations into value or function definitions, on basis of arity
- Makes fatbar and fail pattern-matching operators explicit, removes match and commit
- Implements after and before primitives in terms of Time arithmetic
- 
- Replaces act and req by calls to the rts

- Builds Kindle abstract syntax

LambdaLift:
- Abstracts out free local variables from local function definitions and adds corresponding arguments to call sites
- Abstracts out free local variables from struct functions as extra value fields referenced through "this"
- Moves local function definitions to the top level

Kindle2C:
- Makes construction of cyclic data explicit
- Pretty-prints abstract syntax as C code


-}


compileTimber clo ifs (sm,t_file) ti_file c_file h_file
                        = do let Syntax.Module n is _ _ = sm
                             putStrLn ("[compiling "++ t_file++"]")
                             (imps,ifs') <- chaseIfaceFiles clo is ifs
                             let ((htxt,mtxt),ifc) = runM (passes imps sm)
                             encodeCFile ti_file ifc
                             writeFile c_file mtxt
                             writeFile h_file htxt
                             if api clo then do
                                                writeAPI (rmSuffix ".ti" ti_file) ifc
                                                return ((n,ifc):ifs')
                              else return ((n,ifc):ifs')
  where passes imps par = do (e0,e1,e2,e3,e4) <- initEnvs imps
                             -- e4 :: Core.Module contains types, type signatures and equations from imported modules
                             --                   that are not included in e3. Presently not used.
                             (d1,a0) <- pass clo (desugar1 e0)                Desugar1  par
                             rn      <- pass clo (renameM e1)                 Rename    d1
                             d2      <- pass clo desugar2                     Desugar2  rn
                             co      <- pass clo syntax2core                  S2C       d2
                             kc      <- pass clo (kindcheck e2)               KCheck    co
                             tc      <- pass clo (typecheck e2)               TCheck    kc
                             rd      <- pass clo (termred e2)                 Termred   tc
                             sc      <- pass clo (scp clo e2)                 SCP       rd
                             tc2     <- pass clo (typecheck2 e2)              Type2     sc
                             (ki,ds) <- pass clo (core2kindle e2 e3)          C2K       tc2
                             ki'     <- pass clo (kindlered e3)               Kindlered ki
                             ll      <- pass clo (lambdalift e3)              LLift     ki'
                             pc      <- pass clo (prepare4c e2 e3)            Prepare4C ll
                             c       <- pass clo kindle2c                     K2C       pc
                             return (c,ifaceMod a0 tc2 ds)

        
pass clo m p a          = do -- tr ("Pass " ++ show p ++ " ...")
                             r <- m a
                             Monad.when (dumpAfter clo p) 
                                $ tr ("#### Result after " ++ show p ++ ":\n\n" ++ render (pr r))
                             Monad.when (stopAfter clo p)
                                $ fail ("#### Terminated after " ++ show p ++ ".")
                             return r                                  

makeProg clo cfg root   = do txt <- readFile (root ++ ".t")
                             let ms@(Syntax.Module n is _ _) = runM (parser txt)
                             (imps,ss) <- chaseSyntaxFiles clo is [(n,ms)]
                             let cs = compile_order imps
                                 is = filter nonDummy cs
                             let ps = map (\(n,ii) -> (snd ii,modToPath (str n)++".t")) is ++ [(ms,root++".t")]
                             ifs <- compileAll (clo {shortcut = True}) [] ps
                             r <- checkRoot clo ifs root
                             let basefiles = map (rmSuffix ".t" . snd) ps
                                 c_files   = map (++ ".c") basefiles
                                 o_files   = map ((++ ".o") . rmDirs) basefiles
                             mapM (compileC cfg clo) c_files
                             linkO cfg clo{outfile = root} r o_files
  where nonDummy (_,(_,Syntax.Module n _ _ _)) = str n /= ""


parse clo t_file        = do t_exists <- Directory.doesFileExist t_file
                             Monad.when (not t_exists) (fail ("File " ++ t_file ++ " does not exist."))
                             txt <- readFile t_file
                             let syntax = runM (pass clo parser Parser txt)
                             return (syntax,t_file)

compileAll clo ifs []   = return ifs
compileAll clo ifs (p@(ms,t_file):t_files)
                        = do res <- checkUpToDate clo t_file ti_file c_file h_file (impNames ms)
                             if res then do 
                                 putStrLn ("[skipping " ++ t_file ++ " (output is up to date)]")
                                 compileAll clo ifs t_files
                              else do
                                 ifs' <- compileTimber clo ifs (longName p) ti_file c_file h_file
                                 compileAll clo ifs' t_files
  where base            = rmSuffix ".t" t_file
        ti_file         = base ++ ".ti"
        c_file          = base ++ ".c"
        h_file          = base ++ ".h"
        qm              = takeBaseName t_file
        longName (Syntax.Module m a b c,t)
          |reverse(takeWhile (/= '.') (reverse qm))==str m = (Syntax.Module (name0 qm) a b c,t_file)
          |otherwise = errorIds "Module name not last constructor id in file name" [m]

checkUpToDate clo t_file ti_file c_file h_file imps
  | shortcut clo        = do ti_exists <- Directory.doesFileExist ti_file
                             c_exists  <- Directory.doesFileExist c_file
                             h_exists  <- Directory.doesFileExist h_file
                             if not ti_exists || not c_exists || not h_exists then 
                                 return False 
                              else do
                                 t_time  <- Directory.getModificationTime t_file
                                 ti_time <- Directory.getModificationTime ti_file
                                 c_time  <- Directory.getModificationTime c_file
                                 h_time  <- Directory.getModificationTime h_file
                                 ti_OKs <- mapM (tiOK ti_time) imps
                                 return (t_time < ti_time && t_time < c_time && t_time < h_time && and ti_OKs)
  | otherwise           = return False
  where tiOK ti_time1 n = do let ti_file = modToPath (str n) ++ ".ti"
                             ti_exists <- Directory.doesFileExist ti_file
                             if (not ti_exists) then do
                                let lti_file = Config.libDir clo ++ "/" ++ modToPath (str n) ++ ".ti"
                                lti_exists <- Directory.doesFileExist lti_file
                                if (not lti_exists) then
                                   internalError0 ("Cannot find interface file " ++ ti_file)
                                 else do lti_time <- Directory.getModificationTime lti_file
                                         return (lti_time <= ti_time1)
                               -- return True  -- library module
                               else do ti_time <- Directory.getModificationTime ti_file
                                       return (ti_time <= ti_time1)
------------------------------------------------------------------------------
{-
  Debugging the compiler with the ghci debugger:

  Build and install the compiler somewhere. Once that is done, run the 
  following commands in the timber source repository:

  $ cd src
  $ ghci -fglasgow-exts Main.hs -i../dist/build/timberc/timberc-tmp
  
  This should give you a ghci prompt, type the following (replace PREFIX and
  VERSION with the actual values):

  > :set -fbreak-on-exception
  > :trace main2 ["--datadir", "PREFIX/share/timberc-VERSION", "-v", "file.t"]
 
  <CRASH>

  > :history
    -1  : handleError (Main.hs:346:48-57)
    -2  : handleError (Main.hs:346:10-57)
    -3  : handleError (Main.hs:346:2-58)
    -4  : handleError (Main.hs:(345,31)-(347,14))
    -5  : handleError (Main.hs:(345,0)-(347,14))
    -6  : addDecls (Kindlered.hs:51:62-70)
    -7  : Name.$f7 (Name.hs:525:47-51)
    -8  : Name.$f7 (Name.hs:(518,2)-(525,51))
    -9  : findSel (Kindlered.hs:53:99-103)
    ...
-}


main                = do args <- getArgs
                         main2 args

-- | We have the second entry point so ghci/hugs users can call
-- | main2 directly with arguments.

main2 args          = do (clo, files) <- Exception.catch (cmdLineOpts args)
                                         fatalErrorHandler
                         cfg          <- Exception.catch (readCfg clo)
                                         fatalErrorHandler

                         let t_files  = filter (".t"  `isSuffixOf`) files
                             i_files  = filter (".ti" `isSuffixOf`) files
                             o_files  = filter (".o" `isSuffixOf`) files
                             badfiles = files \\ (t_files++i_files++o_files)
                         
                         Monad.when (not (null badfiles)) $ do
                             fail ("Bad input files: " ++ showids badfiles)
                         
                         mapM (listIface clo) i_files
--                         Monad.when (null t_files) stopCompiler
                         
                         ps <- mapM (parse clo) t_files
                         ifs <- compileAll clo [] ps `Exception.catch` fatalErrorHandler
                         Monad.when (stopAtC clo) stopCompiler
                         
                         let root = make clo
                         Monad.when (root/="") (makeProg clo cfg root)

                         let basefiles = map (rmSuffix ".t") t_files
                             c_files   = map (++ ".c") basefiles
                         mapM (compileC cfg clo) c_files
                         Monad.when (stopAtO clo) stopCompiler

                         let basenames = map rmDirs basefiles
                             o_files'   = map (++ ".o") basenames
                         Monad.when(not (null basenames)) (do r <- checkRoot clo ifs (last basenames)
                                                              linkO cfg clo r (o_files ++ o_files'))

                         return ()


checkRoot clo ifs def       = do if1 <- getIFile rootMod
                                 if2 <- getIFile rtsMod 
                                 let ts = tEnv if2
                                     ke = Core.ksigsOf ts
                                     ds = Core.tdefsOf ts
                                     te = Core.tsigsOf (valEnv if1)
                                 case lookup rootT ke of
                                     Nothing   -> fail ("Cannot locate RootType in module " ++ rtsMod)
                                     Just Star -> case lookup rootT ds of
                                        Nothing                -> internalError0 "checkRoot"
                                        Just (Core.DType _ t') -> checkRoot' te t'
                                        Just _                 -> checkRoot' te (Core.TId rootT)
                                     Just _    -> fail ("Bad RootType in module " ++ rtsMod)
                                         
  where rtsMod              = target clo
        (r,rootMod)         = splitQual (root clo) def
        rootN               = qName rootMod (name0 r)
        rootT               = qName rtsMod (name0 "RootType")
        checkRoot' te t0    = case [ (n,sc) | (n,sc) <- te, n == rootN ] of
                                [(n,sc)]  -> if Core.sameType sc t0
                                            then return n
                                            else fail ("Incorrect root type: " ++ render (pr sc) ++ "; should be " ++ render(pr t0))
                                _ -> fail ("Cannot locate root " ++ (root clo) ++ " in module " ++ rootMod)
        getIFile m          = case lookup (name0 m) ifs of
                                Just ifc -> return ifc
                                Nothing -> do (ifc,_) <- decodeModule clo (modToPath m ++ ".ti")
                                              return ifc

------------------------------------------------------------------------------

-- | Catch internal errors, print them and then abortCompiler.
fatalErrorHandler   :: TimbercException -> IO a
fatalErrorHandler e =  do putStrLn (show e)
                          abortCompiler

-- Getting import info ---------------------------------------------------------


chaseImps                         :: (String -> IO (a,String)) -> (Bool -> a -> [(Name,Bool)]) -> String -> [Syntax.Import] -> Map Name a -> IO (Map Name (ImportInfo a), Map Name a)
chaseImps readModule iNames suff imps ifs              
                                     = do bms <- mapM (readImport ifs) imps
                                          let newpairs = [p | (p,True) <- bms]
                                              ifs1 =  [(c,ifc) | (c,(_,ifc)) <- newpairs] ++ ifs
                                          chaseRecursively ifs1 (map fst bms) (concat [ iNames b c |  ((_,(b,c)),_) <- bms ])
  where readIfile ifs c              = case lookup c ifs of
                                        Just ifc -> return (ifc,False)
                                        Nothing -> do (ifc,f) <- readModule f
                                                      return (ifc,True)
                                          where f = modToPath(str c) ++ suff
        readImport ifs (Syntax.Import b c) 
                                     = do (ifc,isNew) <- readIfile ifs c
                                          return ((c,(b,ifc)),isNew)
        chaseRecursively ifs ms []= return (ms,ifs)
        chaseRecursively ifs ms (p@(r,unQual) : rs)
                                     = case lookup r ms of
                                          Just (b,_) 
                                            | not b && unQual -> chaseRecursively ifs (update r ms) rs -- found import chain; previously only use chain
                                            | otherwise -> chaseRecursively ifs ms rs
                                          Nothing -> do (ifc,isNew)  <- readIfile ifs r
                                                        chaseRecursively (if isNew then (r,ifc) : ifs else ifs) ((r,(unQual,ifc)) : ms) (rs ++ iNames unQual ifc)
        update r ((r',(_,ifc)) : ps)
             | r == r'              = (r,(True,ifc)) : ps
        update r (p : ps)          = p : update r ps
        update _ []                = internalError0 "Main.update: did not find module"
        


chaseIfaceFiles clo                 = chaseImps (decodeModule clo) impsOf2 ".ti"
  where impsOf2 b i                 = map (\(b',c) -> (c,b && b')) (impsOf i)
                                          
impName (Syntax.Import b c)          = c
impNames (Syntax.Module _ is _ _)    = map impName is

impNames2 b (Syntax.Module _ is _ _)    
                                     = map  (\(Syntax.Import b' c) -> (c,b && b')) is

chaseSyntaxFiles clo                 = chaseImps readSyntax impNames2 ".t"
  where readSyntax f                 = (do cont <- readFile f
                                           let sm = runM (parser cont)
                                           return (sm,f)) `catch` (\ e -> do  let libf = Config.libDir clo ++ "/" ++ f
                                                                              t_exists <- Directory.doesFileExist libf
                                                                              if t_exists 
                                                                                then return (Syntax.Module (name0 "") [] [] [],libf) 
                                                                                else fail ("File "++ f ++ " does not exist."))
                                                                              

transImps (_,ifc)                  = map snd (impsOf ifc)

compile_order imps                   = case topSort (impNames . snd)  imps of 
                                         Left ms  -> errorIds "Mutually recursive modules" ms
                                         Right is -> is


