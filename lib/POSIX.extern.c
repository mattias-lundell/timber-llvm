// The Timber compiler <timber-lang.org>
// 
// Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 
// 3. Neither the names of the copyright holder and any identified
//    contributors, nor the names of their affiliations, may be used to 
//    endorse or promote products derived from this software without 
//    specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#include <fcntl.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include "POSIX.h"
#include "rts.h"
#define SOCKHANDLER   CLOS2
#define HANDLER       CLOS3
#define ACTION        CLOS2
#define FILE2CLOSABLE l_File_POSIX_Closable_POSIX_POSIX
#define RFILE2FILE    l_RFile_POSIX_File_POSIX_POSIX
#define WFILE2FILE    l_WFile_POSIX_File_POSIX_POSIX
#define CONN2CLOSABLE l_Connection_POSIX_Closable_POSIX_POSIX
#define SOCK2CLOSABLE l_Socket_POSIX_Closable_POSIX_POSIX

#define ADD_RDTABLE(desc,act) {rdTable[desc] = act; FD_SET(desc,&readUsed); }
#define ADD_WRTABLE(desc,act) {wrTable[desc] = act; FD_SET(desc,&writeUsed); }

#define CLR_RDTABLE(desc) {rdTable[desc] = NULL; FD_CLR(desc, &readUsed);}
#define CLR_WRTABLE(desc) {wrTable[desc] = NULL; FD_CLR(desc, &writeUsed);}

#define GC_STD   0  //********** fix: defined in gc.c

// --------- Socket data stored in global array sockTable, indexed by descriptor -------------------------------------------------

struct SockData {
  WORD *GCINFO;
  struct sockaddr_in addr;              // address of remote peer
  SOCKHANDLER handler;      
  Connection_POSIX conn;                // open connection; needed when closing.
};

WORD __GC__SockData[] = { WORDS(sizeof(struct SockData)), GC_STD, WORDS(offsetof(struct SockData,handler)), 0 };  

typedef struct SockData *SockData;


// -------- Global variables ---------------------------------------------------

/*

  Bit n in readUsed is set iff
  - we have used installR to install a callback for the RFile with descriptor n; the callback is rdTable[n] OR
  - n is a (server) socket, we have called listen and are waiting for clients to connect; handler is sockTable[i] OR
  - n is a socket with an established connection; deliver method of the Destination is rdTable[i] and sockTable[i] contains
    SockData (needed for future closing message).

  Bit n in writeUsed is set iff
  - we have used installW to install a callback for the WFile with descriptor n; the callback is wrTable[i] OR
  - n is a (client) socket, we have called connect and are waiting for connection with server to be established; sockTable[i]
    contains SockData from which we can construct the handler when connection is set up.

  In all other cases, bits are cleared and array entries are NULL.

  eventLoop runs the indefinite loop that repetitively blocks on select; each change to the above data strucures is 
  reported to the calling thread through a SIGSELECT signal, so that select parameters can be adapted accordingly.

  maxDesc is an upper bound on the highest descriptor in use; updated when opening, but currently not decreased on closing.

*/


fd_set readUsed, writeUsed;

HANDLER rdTable[FD_SETSIZE] ;
ACTION  wrTable[FD_SETSIZE] ;
SockData sockTable[FD_SETSIZE];

int envRootsDirty;

struct Msg evMsg = { NULL, 0, { 0, 0 }, { INF, 0 }, NULL };

Thread eventThread = NULL; 

pthread_mutex_t envmut;

int maxDesc = 2;

void startLoop();

//---------- Utilities ---------------------------------------------------------

Host_POSIX mkHost(struct sockaddr_in addr) {
  _Host_POSIX host; NEW(_Host_POSIX, host, WORDS(sizeof(struct _Host_POSIX)));
  host->GCINFO = __GC___Host_POSIX;
  host->a = getStr(inet_ntoa(addr.sin_addr));
  return (Host_POSIX)host;
}

Port_POSIX mkPort (struct sockaddr_in addr) {
  _Port_POSIX port; NEW(_Port_POSIX, port, WORDS(sizeof(struct _Port_POSIX)));
  port->GCINFO = __GC___Port_POSIX;
  port->a = ntohs(addr.sin_port); 
  return (Port_POSIX)port;
}

void netError(Int sock,char *message);

// -------- Closable -----------------------------------------------------------

struct DescClosable {
  WORD *GCINFO;
  UNIT (*close_POSIX) (Closable_POSIX, Int);
  int descriptor;
};
 
WORD __GC__DescClosable[] = { WORDS(sizeof(struct DescClosable)), GC_STD, 0 };

typedef struct DescClosable *DescClosable;

struct CloseMsg;
typedef struct CloseMsg *CloseMsg;

struct CloseMsg {
  WORD *GCINFO;
  UNIT (*Code)(CloseMsg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
  int descriptor;
};

WORD __GC__CloseMsg[] = { WORDS(sizeof(struct CloseMsg)), GC_STD, 0 };   // Field "next" is custom handled by the GC


UNIT close_fun (Closable_POSIX this, Int dummy) {
  DISABLE(envmut);
  int desc = ((DescClosable)this)->descriptor;
  close(desc);
  CLR_RDTABLE(desc);
  CLR_WRTABLE(desc);
  sockTable[desc] = NULL;
  if (eventThread) pthread_kill(eventThread->id, SIGSELECT);
  ENABLE(envmut);
  return (UNIT)0;
}

Closable_POSIX new_Closable (int desc) {
  DescClosable res;
  NEW(DescClosable, res, WORDS(sizeof(struct DescClosable)));
  res->GCINFO = __GC__DescClosable;
  res->close_POSIX = close_fun;
  res->descriptor = desc;
  return (Closable_POSIX)res;
}

// -------- File ---------------------------------------------------------------

Int seek_fun (File_POSIX this, Int off, Int dummy) {
  DISABLE(envmut);
  Int res, mode;
  if (off >= 0) 
    mode = SEEK_SET;
  else {
    mode = SEEK_END;
    off++;
  }
  res = lseek(((DescClosable)this->FILE2CLOSABLE)->descriptor,off,mode);
  ENABLE(envmut);
  return res;
}

File_POSIX new_File (int desc) {
  File_POSIX res; NEW(File_POSIX, res, WORDS(sizeof(struct File_POSIX)));
  res->GCINFO = __GC__File_POSIX;
  res->FILE2CLOSABLE = new_Closable(desc);
  res->seek_POSIX = seek_fun;
  return res;
}

// --------- RFile -------------------------------------------------------------

LIST read_descr (int descr) {
  char buf[1024];
  LIST xs = (LIST)0;
  LIST xslast = (LIST)0;
  LIST res = (LIST)0;
  LIST reslast = (LIST)0;
  int r;
  while (1) {
    xs = (LIST)0;
    xslast = (LIST)0;
    r = read(descr, buf, 1023);
    if (r <= 0) {
      if (reslast != (LIST)0) ((CONS)reslast)->b = (LIST)0;
      return res;
    }
    while (r) {
      CONS n; NEW(CONS, n, WORDS(sizeof(struct CONS)));
      if (xslast==(LIST)0) xslast = (LIST)n;
      n->GCINFO = __GC__CONS+5;                         // POLY instance is a scalar
      n->a = (POLY)(Int)buf[--r];
      n->b = xs;
      xs = (LIST)n;
    }
    if (res==(LIST)0) 
      res = xs; 
    else 
      ((CONS)reslast)->b = xs;
    reslast = xslast;
  }
}


LIST read_fun (RFile_POSIX this, Int dummy) {
  return read_descr(((DescClosable)this->RFILE2FILE->FILE2CLOSABLE)->descriptor);
}

UNIT installR_fun (RFile_POSIX this, HANDLER hand, Int dummy) {
  DISABLE(envmut);
  Int desc = ((DescClosable)this->RFILE2FILE->FILE2CLOSABLE)->descriptor;
  Int active = FD_ISSET(desc,&readUsed);
  ADD_RDTABLE(desc,hand);
  maxDesc = desc > maxDesc ? desc : maxDesc;  
  if (!eventThread) startLoop();
  else if (!active) pthread_kill(eventThread->id, SIGSELECT);
  ENABLE(envmut);
  return (UNIT)0;
}

RFile_POSIX new_RFile(int desc) {
    RFile_POSIX rf; NEW(RFile_POSIX, rf, sizeof(struct RFile_POSIX));
    rf->GCINFO = __GC__RFile_POSIX;
    rf->RFILE2FILE = new_File(desc);
    rf->read_POSIX = read_fun;
    rf->installR_POSIX = installR_fun;
    return rf;
}

// ----------- WFile -----------------------------------------------------------

int write_fun (WFile_POSIX this, LIST xs, Int dummy) {
  char buf[1024];
  int res = 0;
  while (xs) {
    int len = 0;
    while (xs && len < 1024) {
      buf[len++] = (Char)(Int)((CONS)xs)->a;
      xs = ((CONS)xs)->b;
    }
    if (len<1024) buf[len] = 0;
    int r = write(((DescClosable)this->WFILE2FILE->FILE2CLOSABLE)->descriptor, buf, len);
    if (r < 0) return res;
    res += r;
  }
  return res;
}

UNIT installW_fun (WFile_POSIX this, ACTION act, Int dummy) {
  DISABLE(envmut);
  Int desc = ((DescClosable)this->WFILE2FILE->FILE2CLOSABLE)->descriptor;
  Int active = FD_ISSET(desc,&writeUsed);
  ADD_WRTABLE(desc,act);
  envRootsDirty = 1;
  maxDesc = desc > maxDesc ? desc : maxDesc;  
  if (!eventThread) startLoop();
  else if (!active) pthread_kill(eventThread->id, SIGSELECT);
  ENABLE(envmut);
  return (UNIT)0;
}

WFile_POSIX new_WFile(int desc) {
    WFile_POSIX wf; NEW(WFile_POSIX, wf, sizeof(struct WFile_POSIX));
    wf->GCINFO = __GC__WFile_POSIX;
    wf->WFILE2FILE = new_File(desc);
    wf->write_POSIX = write_fun;
    wf->installW_POSIX = installW_fun;
    return wf;
}

// ------------ Env ------------------------------------------------------------

UNIT exit_fun (Env_POSIX this, Int n, Int dummy) {
  DISABLE(envmut);
  DISABLE(rts);
  exit(n);
}

Maybe_Prelude open_fun (LIST path, int oflag) {
  char buf[1024];
  int len = 0;
  while (path && len < 1024) {
    buf[len++] = (Char)(Int)((CONS)path)->a;
    path = ((CONS)path)->b;
  }
  buf[len] = 0;
  int descr = open(buf,oflag,S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
  if (descr < 0) return (Maybe_Prelude)0;
  _Just_Prelude res; NEW(_Just_Prelude, res, WORDS(sizeof(struct _Just_Prelude)));
  res->GCINFO = __GC___Just_Prelude+0;           // POLY instance is a pointer
  res->a = (POLY)new_File(descr);
  return (Maybe_Prelude)res;
}


Maybe_Prelude openR_fun (Env_POSIX this, LIST path, Int dummy) {
  DISABLE(envmut);
  Maybe_Prelude f = open_fun(path,O_RDONLY);
  if (f) {
    RFile_POSIX rf; NEW(RFile_POSIX, rf, WORDS(sizeof(struct RFile_POSIX)));
    rf->GCINFO = __GC__RFile_POSIX;
    rf->RFILE2FILE = (File_POSIX)((_Just_Prelude)f)->a;
    rf->read_POSIX = read_fun;
    rf->installR_POSIX = installR_fun;
    ((_Just_Prelude)f)->a = (POLY)rf;
  }
  ENABLE(envmut);
  return f;
}

Maybe_Prelude openW_fun (Env_POSIX this, LIST path, Int dummy) {
  DISABLE(envmut);
  Maybe_Prelude f =  open_fun(path,O_WRONLY | O_CREAT | O_TRUNC);
  if (f) {
    WFile_POSIX wf; NEW(WFile_POSIX, wf, WORDS(sizeof(struct WFile_POSIX)));
    wf->GCINFO = __GC__WFile_POSIX;
    wf->WFILE2FILE = (File_POSIX)((_Just_Prelude)f)->a;
    wf->write_POSIX = write_fun;
    wf->installW_POSIX = installW_fun;
    ((_Just_Prelude)f)->a = (POLY)wf;
  }
  ENABLE(envmut);
  return f;
} 

// ---------- Sockets ----------------------------------------------------------

Socket_POSIX new_Socket (Int sock) {
  Socket_POSIX res;
  NEW (Socket_POSIX, res, WORDS(sizeof(struct Socket_POSIX)));
  res->GCINFO = __GC__Socket_POSIX;
  res->SOCK2CLOSABLE = new_Closable(sock);
  res->inFile_POSIX = new_RFile(sock);
  res->outFile_POSIX = new_WFile(sock);  
  struct sockaddr_in addr = sockTable[sock]->addr;
  res->remoteHost_POSIX = mkHost(addr);
  res->remotePort_POSIX = mkPort(addr);
  return res;
}


Int new_socket (SOCKHANDLER handler) {
  SockData d; 
  int sock = socket(PF_INET,SOCK_STREAM,0);
  fcntl(sock,F_SETFL,O_NONBLOCK);
  maxDesc = sock > maxDesc ? sock : maxDesc;  
  NEW(SockData,d,WORDS(sizeof(struct SockData)));
  d->GCINFO =__GC__SockData;
  d->handler = handler;
  sockTable[sock] = d;
  envRootsDirty = 1;
  return sock;
}  

void netError (Int sock, char *message) {
  SOCKHANDLER handler = sockTable[sock]->handler;
  Connection_POSIX conn = (Connection_POSIX)handler->Code(handler,(POLY)new_Socket(sock),(POLY)0);
  envRootsDirty = 1;
  conn->neterror_POSIX(conn,getStr(message),Inherit,Inherit);
}

void setupConnection (Int sock) {
  SOCKHANDLER handler = sockTable[sock]->handler;
  Connection_POSIX conn = (Connection_POSIX)handler->Code(handler,(POLY)new_Socket(sock),(POLY)0);
  sockTable[sock]->conn = conn;
  envRootsDirty = 1;
  conn->established_POSIX(conn,Inherit,Inherit);
}

int mkAddr (Int sock, Host_POSIX host, struct in_addr *addr) {
  LIST h = ((_Host_POSIX)host)->a;
  char buf[1024];
  int len = 0;
  Int hostid;
  struct hostent *ent;
  while (h && len < 1024) {
    buf[len++] = (Char)(Int)((CONS)h)->a;
    h = ((CONS)h)->b;
  }
  buf[len] = 0;
  // We assume gethostbyname will not block...
  ent = gethostbyname(buf);
  if(ent==NULL) {
    netError(sock,"Name lookup error");
    return -1;
  }
  else {
    memcpy(&hostid, ent->h_addr_list[0], sizeof hostid);
    addr->s_addr = hostid;
    return 0;
  }
}

UNIT connect_fun (Sockets_POSIX this, Host_POSIX host, Port_POSIX port, SOCKHANDLER handler, Int dummy) {
  DISABLE(envmut);
  struct sockaddr_in addr;
  struct in_addr iaddr;
  int sock = new_socket(handler);
  if (mkAddr(sock, host, &iaddr) == 0) {
    addr.sin_addr = iaddr;
    addr.sin_port = htons(((_Port_POSIX)port)->a);
    addr.sin_family = AF_INET;
    sockTable[sock]->addr = addr;
    if (connect(sock,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0) {// couldn't connect immediately, 
      if (errno==EINPROGRESS)                                                // so check if attempt continues asynchronously.
	FD_SET(sock,&writeUsed);
      else
	netError(sock,"Connect failed");
    }
    else {
      setupConnection(sock);
    }
    if (!eventThread) startLoop();
    else pthread_kill(eventThread->id, SIGSELECT);
  }
  ENABLE(envmut);
  return (UNIT)0;
}


Closable_POSIX listen_fun (Sockets_POSIX this, Port_POSIX port, SOCKHANDLER handler, Int dummy) {
  DISABLE(envmut);
  struct sockaddr_in addr;
  int sock = new_socket(handler);
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(((_Port_POSIX)port)->a);
  addr.sin_family = AF_INET;
  if (bind(sock,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
    perror("bind failed");
  listen(sock,5);
  FD_SET(sock,&readUsed);
  if (!eventThread) startLoop();
  else pthread_kill(eventThread->id, SIGSELECT);
  ENABLE(envmut);
  return new_Closable(sock);
}

// ---------- Global env object ------------------------------------------------

// Note: all the following structs lie outside the garbage collected heap, and are therefore marked with a gcinfo = 0.

struct DescClosable stdin_cl    = { 0, close_fun, 0 };
struct DescClosable stdout_cl   = { 0, close_fun, 1 };

struct File_POSIX stdin_file    = { 0, (Closable_POSIX)&stdin_cl, seek_fun };
struct File_POSIX stdout_file   = { 0, (Closable_POSIX)&stdout_cl, seek_fun };

struct RFile_POSIX stdin_rfile  = { 0, &stdin_file, read_fun, installR_fun };
struct WFile_POSIX stdout_wfile = { 0, &stdout_file, write_fun, installW_fun };

struct Sockets_POSIX tcp        = { 0, connect_fun, listen_fun };

struct Internet_POSIX inet      = { 0, &tcp };

struct Time startTime;

struct Env_POSIX env_struct     = { 0, exit_fun,  NULL, &stdin_rfile, &stdout_wfile,
                                    openR_fun, openW_fun, &startTime, &inet };


void kill_handler () {
    return;
}



Env_POSIX env                   = &env_struct;

// ------- Copying for gc -----------------------------------------------

void scanEnvRoots () {
        int i = 0;
        DISABLE(envmut);
        while (i<maxDesc+1) {
                if (rdTable[i]) rdTable[i] = (HANDLER)copy((ADDR)rdTable[i]);
                if (wrTable[i]) wrTable[i] = (ACTION)copy((ADDR)wrTable[i]);
                if (sockTable[i]) sockTable[i] = (SockData)copy((ADDR)sockTable[i]);
                i++;
                ENABLE(envmut);
                DISABLE(envmut);
        }
        ENABLE(envmut);
}

struct FunList scanner = {scanEnvRoots, NULL};


// --------- Event loop ----------------------------------------------

void *eventLoop (void *arg) {
    Thread current_thread = (Thread)arg;
    pthread_setspecific(current_key, current_thread);
    struct sched_param param;
    param.sched_priority = current_thread->prio;
    pthread_setschedparam(current_thread->id, SCHED_RR, &param);

    sigset_t one_sig;
    sigemptyset(&one_sig);
    sigaddset(&one_sig, SIGSELECT);
    pthread_sigmask(SIG_UNBLOCK, &one_sig, NULL);

    DISABLE(envmut);
    fd_set readFds, writeFds;
    int i;
    while(1) {
        readFds = readUsed;
        writeFds = writeUsed;
        ENABLE(envmut);
        int r = select(maxDesc+1, &readFds, &writeFds, NULL, NULL);
        DISABLE(envmut);
        if (r >= 0) {
            TIMERGET(evMsg.baseline);
            for(i=0; i<maxDesc+1; i++) {
	            if (FD_ISSET(i, &readFds)) {
	                if (rdTable[i]) {
	                    LIST inp = read_descr(i);
	                    if (inp) {
	                        rdTable[i]->Code(rdTable[i],(POLY)inp,(POLY)Inherit,(POLY)Inherit);
	                    }
	                    else if (sockTable[i]) { //we got a close message from peer on connected socket
                                Closable_POSIX cl = sockTable[i]->conn->CONN2CLOSABLE;
	                        cl->close_POSIX(cl,0);
	                        close(i);
	                        CLR_RDTABLE(i);
	                        sockTable[i] = NULL;
	                    }
	                } else if (sockTable[i]) { //listening socket received a connect request; we will accept
	                    socklen_t len = sizeof(struct sockaddr);
	                    struct sockaddr_in addr;
	                    Int sock = accept(i,(struct sockaddr *)&addr,&len);
	                    fcntl(sock,F_SETFL,O_NONBLOCK);
	                    NEW(SockData,sockTable[sock],WORDS(sizeof(struct SockData)));
	                    sockTable[sock]->handler = sockTable[i]->handler;
	                    sockTable[sock]->addr = addr;
	                    maxDesc = sock > maxDesc ? sock : maxDesc;
	                    setupConnection(sock);
	                }
	            }
	            if (FD_ISSET(i, &writeFds)) {
	                if (wrTable[i]) {
	                    wrTable[i]->Code(wrTable[i],(POLY)Inherit,(POLY)Inherit);
	                } else if (sockTable[i]) { //delayed connection has been accepted or has failed
	                    int opt;
	                    socklen_t len = sizeof(int);
	                    FD_CLR(i,&writeUsed);
	                    if (getsockopt(i,SOL_SOCKET,SO_ERROR, (void *)&opt, &len) < 0)
	                        perror("getsockopt failed");
	                    if (opt) {
	                        netError(i,"Connection failed");
	                    } else {
	                        setupConnection(i);
	                    }
	                }
	            }
            }
        }
    }
}

// --------- Initialization ----------------------------------------------------


Env_POSIX posix_POSIX(World w, Int dummy) {
  if (!env->argv_POSIX) {
    pthread_mutex_init(&envmut, &glob_mutexattr);
  
    FD_ZERO(&readUsed);
    FD_ZERO(&writeUsed);
  
    struct sigaction act;
    act.sa_flags = 0;
    sigemptyset( &act.sa_mask );
    act.sa_handler = kill_handler;
    sigaction( SIGSELECT, &act, NULL );

    int argc = getArgc();
    char **argv = getArgv();
  
    Array arr; NEW(Array,arr,WORDS(sizeof(struct Array))+argc);
    arr->GCINFO = __GC__Array0;
    arr->size = argc;
    int i;
    for (i=0; i<argc; i++)
        arr->elems[i] = (POLY)getStr(argv[i]);
    env->argv_POSIX = arr;
  
    fcntl(0, F_SETFL, O_NONBLOCK);
    fcntl(1, F_SETFL, O_NONBLOCK);

    TIMERGET(evMsg.baseline);
    startTime.sec = evMsg.baseline.tv_sec;
    startTime.usec = evMsg.baseline.tv_usec;

    addRootScanner(&scanner);
  }
  return env;
}

// --------- Start event loop ----------------------------------------------------

void startLoop () {

    eventThread = newThread(&evMsg,sched_get_priority_max(SCHED_RR),eventLoop,0);
}
