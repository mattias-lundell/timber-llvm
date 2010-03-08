module LLVMPrimitives where

import LLVMKindle

genPrimitives = do
  addStruct "LIST"  [("GCINFO",poly)]
  addStruct "CONS"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", liststruct)]
  addStruct "TUP2"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", poly)] 
  addStruct "TUP3"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", poly), 
                     ("c", poly)] 
  addStruct "TUP4"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", poly), 
                     ("c", poly), 
                     ("d", poly)]
  addStruct "TUPLE" [("GCINFO",poly), 
                     ("size", int), 
                     ("elems",array 0 poly)]
  addStruct "CLOS1" [("GCINFO",poly), 
                     ("Code", (ptr (fun poly [clos1struct, 
                                               poly])))]
  addStruct "CLOS2" [("GCINFO",poly), 
                     ("Code", (ptr (fun poly [clos2struct, 
                                               poly, 
                                               poly])))]
  addStruct "CLOS3" [("GCINFO",poly), 
                     ("Code", (ptr (fun poly [clos3struct, 
                                               poly, 
                                               poly, 
                                               poly])))]
  addStruct "CLOS"  [("GCINFO",poly),
                     ("Code", (ptr (fun void [])))]
  
  addStruct "PTHREAD_DESCR_STRUCT" [("a",opaque)]
  addStruct "PTHREAD_FASTLOCK" [("a",int),
                                ("b",int)]
  addStruct "PTHREAD_MUTEX_T" [("a",int),
                               ("b",int),
                               ("c",ptr (struct "PTHREAD_DESCR_STRUCT")),
                               ("d",int),
                               ("e",struct "PTHREAD_FASTLOCK")]
  addStruct "Ref"   [("GCINFO",   poly),
                     ("mut",      struct "PTHREAD_MUTEX_T"),
                     ("STATE",    poly)]
  addStruct "AbsTime" [("a",     int),
                       ("b",     int)]
  addStruct "Msg"   [("GCINFO",   (poly)),
                     ("Code",     (ptr (fun int [msgstruct]))),
                     ("baseline", struct "AbsTime"),
                     ("deadline", struct "AbsTime"),
                     ("next",     msgstruct)]
  addStruct "Time"  [("GCINFO",   (poly)),
                     ("sec",      int),
                     ("usec",     int)]
  addStruct "EITHER" [("GCINFO", poly),
                      ("Tag", int)]
  addStruct "LEFT"   [("GCINFO", poly),
                      ("Tag", int),
                      ("a", poly)]
  addStruct "RIGHT"  [("GCINFO", poly),
                      ("Tag", int),
                      ("a", poly)]
  addStruct "FloatCast" [("float", float)]
  addStruct "WORLD" [("",opaque)] 
  addStruct "Array" [("GCINFO",poly),
                     ("size", int),
                     ("elems", array 0 poly)]
  addStruct "Timer" [("GCINFO",poly),
                     ("reset",ptr (fun unit [timerstruct,int])),
                     ("sample",ptr (fun timestruct [timerstruct,int]))]

  addExternalFun "new"     void [ptr (ptr int), int]
  addExternalFun "LOCK"    (ptr int) [ptr int]
  addExternalFun "UNLOCK"  bit8 [ptr int]
  addExternalFun "INITREF" void [refstruct]
  addExternalFun "ASYNC"   bit8 [msgstruct,timestruct,timestruct]
  addExternalFun "primTimeMin" timestruct [timestruct,timestruct];
  addExternalFun "primTimePlus" timestruct [timestruct,timestruct];
  addExternalFun "primTimeMinus" timestruct [timestruct,timestruct];
  addExternalFun "primTimeEQ" bool [timestruct,timestruct];
  addExternalFun "primTimeNE" bool [timestruct,timestruct];
  addExternalFun "primTimeLT" bool [timestruct,timestruct];
  addExternalFun "primTimeLE" bool [timestruct,timestruct];
  addExternalFun "primTimeGT" bool [timestruct,timestruct];
  addExternalFun "primTimeGE" bool [timestruct,timestruct];
  addExternalFun "sec" timestruct [int]
  addExternalFun "millisec" timestruct [int]
  addExternalFun "microsec" timestruct [int]
  addExternalFun "secOf" int [timestruct]
  addExternalFun "microsecOf" int [timestruct]
  addExternalFun "RAISE"  void [int]
  addExternalFun "Raise" poly [bit32,int]
  addExternalFun "primRefl" poly [bit32,poly]
  addExternalFun "getStr" liststruct [ptr char]
  addExternalFun "strEq" int [liststruct,liststruct]
  addExternalFun "primListArray" arraystruct [bit32,liststruct]
  addExternalFun "primUniArray" arraystruct [bit32,int,poly]
  addExternalFun "EmptyArray" arraystruct [bit32,int]
  addExternalFun "CloneArray" arraystruct [bit32,arraystruct,int]
  addExternalFun "UpdateArray" arraystruct [bit32,arraystruct,int,poly]
  addExternalFun "primShowFloat" liststruct [float]
  addExternalFun "ABORT" bit8 [bit32,msgstruct,refstruct]
  addExternalFun "primTIMERTERM" timerstruct [int]

  addExternalGC "TUP2"   (array 0 int)
  addExternalGC "TUP3"   (array 0 int)
  addExternalGC "TUP4"   (array 0 int)
  addExternalGC "TUPLE"  (array 0 int)
  addExternalGC "CLOS1"  (array 0 int)
  addExternalGC "CLOS2"  (array 0 int)
  addExternalGC "CLOS3"  (array 0 int)
  addExternalGC "CLOS"   (array 0 int)
  addExternalGC "CONS"   (array 0 int)
  addExternalGC "EITHER" (array 0 int)
  addExternalGC "LEFT"   (array 0 int)
  addExternalGC "RIGHT"  (array 0 int)
  addExternalGC "Msg"    (array 0 int)
  addExternalGC "Timer"  (array 0 int)
  addExternalGC "Ref"    (array 0 int)
  addExternalGC "Time"   (array 0 int)
  addExternalGC "Array"  (array 0 int)
