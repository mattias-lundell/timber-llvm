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

module POSIX where
    
type RootType = World -> Cmd () ()

struct Env where
    exit      :: Int -> Request ()
    argv      :: Array String
    stdin     :: RFile
    stdout    :: WFile
    openR     :: String -> Request (Maybe RFile)
    openW     :: String -> Request (Maybe WFile)
    startTime :: Time
    inet      :: Internet

struct Closable where
    close :: Request ()

struct File < Closable where
    seek  :: Int -> Request Int
    
struct RFile < File where
    read     :: Request String
    installR :: (String -> Action) -> Request ()
    
struct WFile < File where
    write    :: String -> Request Int
    installW :: Action -> Request ()

data Host = Host String
data Port = Port Int

struct Internet where
    tcp :: Sockets

struct Socket < Closable where
    remoteHost :: Host
    remotePort :: Port
    inFile     :: RFile
    outFile    :: WFile

struct Connection < Closable where 
    established :: Action
    neterror    :: String -> Action

struct Sockets where
    connect :: Host -> Port -> (Socket -> Class Connection) -> Request ()
    listen  :: Port -> (Socket -> Class Connection) -> Request Closable

instance showHost :: Show Host
showHost = struct
    show (Host nm) = nm

extern posix :: World -> Class Env
