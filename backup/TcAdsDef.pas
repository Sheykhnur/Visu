//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        File: TcAdsDef.pas                                                                                                           ///
// Description: Prototypes and Definitions for ads communications                                                                      ///
//      Author: ChristophC                                                                                                             ///
//     Created: 25.03.2003                                                                                                             ///
//                                                                                                                                     ///
//                                                                                                                                     ///
// BECKHOFF-Industrieelektronik-GmbH                                                                                                   ///
//                                                                                                                                     ///
// Modifications:                                                                                                                      ///
// 15.04.2003 ChristophC:                                                                                                              ///
// Router callback function type redefinition: TAmsRouterNotificationFunc -> TAmsRouterNotificationFuncEx and                          ///
// PAmsRouterNotificationFunc -> PAmsRouterNotificationFuncEx                                                                          ///
//                                                                                                                                     ///
// 16.10.2007 ChristophC: New ADSSTATE_CONFIG and ADSSTATE_RECONFIG added                                                              ///
//                                                                                                                                     ///
// 15.02.2008 ChristophC: C++ macros (symbol type/info) ported to Pascal functions:                                                    ///
// fADSNOTIFICATION_PDATA(), fPADSSYMBOL_NAME(), fPADSSYMBOL_TYPE(), fPADSSYMBOL_COMMENT(), fPADSSYMBOL_NEXTENTRY()                    ///
// fPADSDATATYPE_NAME(), fPADSDATATYPE_TYPE(), fPADSDATATYPE_COMMENT(), fPADSDATATYPE_ARRAYINFO(), fADSDATATYPE_STRUCTITEM()           ///
// New constants: ADSIGRP_SUMUP_READ, ADSIGRP_SUMUP_WRITE, ADSIGRP_SUMUP_READWRITE, ADSIGRP_SUMUP_READEX added                         ///
//                                                                                                                                     ///
// 31.08.2010 ChristophC: Compiler directive DELPHI_6_OR_NEWER added.                                                                  ///
// IsMultiThread := True; in the initialization section added to allow the memory manager to execute safely                            ///
// if a VCL application has more than one thread. If a second thread attempts to allocate memory,                                      ///
// it is blocked until the first thread exits the memory manager Delphi is thread-safe, but only when the developer tells it to be.    ///
//                                                                                                                                     ///
// 07.09.2010 ChristophC: AnsiString/WideString/Char/PChar compatibility problem in newer Delphi version solved                        ///
//                                                                                                                                     ///
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{$ALIGN OFF}      {Disable aligned records,  All record and class structures are packed!}
unit TcAdsDef;

{$mode objfpc}{$H+}
{MODE ObjectPascal}

interface
uses sysutils,LCLIntf, LCLType, LMessages, Windows;

// Eumerated types with explicitly assigned ordinality supported only in Delphi 6 or newer version
{$IfNDef VER80} // Delphi 1
   {$IfNDef VER90} // Delphi 2
      {$IfNDef VER100} // Delphi 3
         {$IfNDef VER120} // Delphi 4
            {$IfNDef VER130} // Delphi 5
               {$DEFINE DELPHI_6_OR_NEWER}
               {$IfNDef VER140} // Delphi 6
               {$EndIf}
            {$EndIf}
         {$EndIf}
      {$EndIf}
   {$EndIf}
{$EndIf}

type
  PBYTE     =^Byte;
  PWORD     =^Word;
  PLONGWORD =^Longword;
  PLONGBOOL =^LongBool;
  PSINGLE   =^Single;
  PDOUBLE   =^Double;

const
  ANYSIZE_ARRAY       =1;

  ADS_FIXEDNAMESIZE   =16;{Ads server name length}
  {Ads ports}
  AMSPORT_LOGGER      =100;
  AMSPORT_R0_RTIME    =200;
  AMSPORT_R0_TRAC     =(AMSPORT_R0_RTIME+90);
  AMSPORT_R0_IO	      =300;
  AMSPORT_R0_SPS      =400;
  AMSPORT_R0_NC	      =500;
  AMSPORT_R0_ISG      =550;
  AMSPORT_R0_PCS      =600;
  AMSPORT_R0_PLC      =800;
  AMSPORT_R0_PLC_RTS1 =801;
  AMSPORT_R0_PLC_RTS2 =811;
  AMSPORT_R0_PLC_RTS3 =821;
  AMSPORT_R0_PLC_RTS4 =831;

  { ADS reserved index groups }
  ADSIGRP_SYMTAB           = $0000F000;
  ADSIGRP_SYMNAME          = $0000F001;
  ADSIGRP_SYMVAL           = $0000F002;
  ADSIGRP_SYM_HNDBYNAME    = $0000F003;
  ADSIGRP_SYM_VALBYNAME    = $0000F004;
  ADSIGRP_SYM_VALBYHND     = $0000F005;
  ADSIGRP_RELEASE_SYMHND   = $0000F006;
  ADSIGRP_SYM_INFOBYNAME   = $0000F007;
  ADSIGRP_SYM_VERSION      = $0000F008;
  ADSIGRP_SYM_INFOBYNAMEEX = $0000F009;
  ADSIGRP_SYM_DOWNLOAD     = $0000F00A;
  ADSIGRP_SYM_UPLOAD       = $0000F00B;
  ADSIGRP_SYM_UPLOADINFO   = $0000F00C;

  ADSIGRP_SYMNOTE	   = $0000F010;	// notification of named handle

  ADSIGRP_IOIMAGE_RWIB	   = $0000F020;	// read/write input byte(s)
  ADSIGRP_IOIMAGE_RWIX	   = $0000F021;	// read/write input bit
  ADSIGRP_IOIMAGE_RWOB	   = $0000F030;	// read/write output byte(s)
  ADSIGRP_IOIMAGE_RWOX	   = $0000F031;	// read/write output bit
  ADSIGRP_IOIMAGE_CLEARI   = $0000F040;	// write inputs to null
  ADSIGRP_IOIMAGE_CLEARO   = $0000F050;	// write outputs to null

  ADSIGRP_SUMUP_READ       = $0000F080;	// AdsRW  IOffs list size or 0 (=0 -> list size == WLength/3*sizeof(ULONG))
											// W: {list of IGrp, IOffs, Length}
											// if IOffs != 0 then R: {list of results} and {list of data}
											// if IOffs == 0 then R: only data (sum result)

  ADSIGRP_SUMUP_WRITE	   = $0000F081;	// AdsRW  IOffs list size
											// W: {list of IGrp, IOffs, Length} followed by {list of data}
											// R: {list of results}

  ADSIGRP_SUMUP_READWRITE  = $0000F082; // AdsRW  IOffs list size
											// W: {list of IGrp, IOffs, RLength, WLength} followed by {list of data}
											// R: {list of results, RLength} followed by {list of data}

  ADSIGRP_SUMUP_READEX	   = $0000F083;	// AdsRW  IOffs list size
											// W: {list of IGrp, IOffs, Length}
											// R: {list of results, Length} followed by {list of data}

  ADSIGRP_DEVICE_DATA	   = $0000F100;	// state, name, etc...
  ADSIOFFS_DEVDATA_ADSSTATE= $00000000;	// ads state of device
  ADSIOFFS_DEVDATA_DEVSTATE= $00000002;	// device state

  { ADS Error codes }
  ERR_ADSERRS                         = $0700;
  ADSERR_NOERR		              = $0000;

  ADSERR_DEVICE_ERROR		      = ($00+ERR_ADSERRS); // Error class < device error >
  ADSERR_DEVICE_SRVNOTSUPP	      = ($01+ERR_ADSERRS); // Service is not supported by server
  ADSERR_DEVICE_INVALIDGRP 	      = ($02+ERR_ADSERRS); // invalid indexGroup
  ADSERR_DEVICE_INVALIDOFFSET	      = ($03+ERR_ADSERRS); // invalid indexOffset
  ADSERR_DEVICE_INVALIDACCESS	      = ($04+ERR_ADSERRS); // reading/writing not permitted
  ADSERR_DEVICE_INVALIDSIZE	      = ($05+ERR_ADSERRS); // parameter size not correct
  ADSERR_DEVICE_INVALIDDATA	      = ($06+ERR_ADSERRS); // invalid parameter value(s)
  ADSERR_DEVICE_NOTREADY	      = ($07+ERR_ADSERRS); // device is not in a ready state
  ADSERR_DEVICE_BUSY		      = ($08+ERR_ADSERRS); // device is busy
  ADSERR_DEVICE_INVALIDCONTEXT	      = ($09+ERR_ADSERRS); // invalid context (must be InWindows)
  ADSERR_DEVICE_NOMEMORY	      = ($0A+ERR_ADSERRS); // out of memory
  ADSERR_DEVICE_INVALIDPARM           = ($0B+ERR_ADSERRS); // invalid parameter value(s)
  ADSERR_DEVICE_NOTFOUND	      = ($0C+ERR_ADSERRS); // not found (files, ...)
  ADSERR_DEVICE_SYNTAX		      = ($0D+ERR_ADSERRS); // syntax error in comand or file
  ADSERR_DEVICE_INCOMPATIBLE	      = ($0E+ERR_ADSERRS); // objects do not match
  ADSERR_DEVICE_EXISTS		      = ($0F+ERR_ADSERRS); // object already exists
  ADSERR_DEVICE_SYMBOLNOTFOUND	      = ($10+ERR_ADSERRS); // symbol not found
  ADSERR_DEVICE_SYMBOLVERSIONINVALID  = ($11+ERR_ADSERRS); // symbol version invalid
  ADSERR_DEVICE_INVALIDSTATE	      = ($12+ERR_ADSERRS); // server is in invalid state
  ADSERR_DEVICE_TRANSMODENOTSUPP      = ($13+ERR_ADSERRS); // AdsTransMode not supported
  ADSERR_DEVICE_NOTIFYHNDINVALID      = ($14+ERR_ADSERRS); // Notification handle is invalid
  ADSERR_DEVICE_CLIENTUNKNOWN	      = ($15+ERR_ADSERRS); // Notification client not registered
  ADSERR_DEVICE_NOMOREHDLS	      = ($16+ERR_ADSERRS); // no more notification handles
  ADSERR_DEVICE_INVALIDWATCHSIZE      = ($17+ERR_ADSERRS); // size for watch to big
  ADSERR_DEVICE_NOTINIT		      = ($18+ERR_ADSERRS); // device not initialized
  ADSERR_DEVICE_TIMEOUT		      = ($19+ERR_ADSERRS); // device has a timeout
  ADSERR_DEVICE_NOINTERFACE	      = ($1A+ERR_ADSERRS); // query interface failed
  ADSERR_DEVICE_INVALIDINTERFACE      = ($1B+ERR_ADSERRS); // wrong interface required
  ADSERR_DEVICE_INVALIDCLSID	      = ($1C+ERR_ADSERRS); // class ID is invalid
  ADSERR_DEVICE_INVALIDOBJID	      = ($1D+ERR_ADSERRS); // object ID is invalid
  ADSERR_DEVICE_PENDING		      = ($1E+ERR_ADSERRS); // request is pending
  ADSERR_DEVICE_ABORTED		      = ($1F+ERR_ADSERRS); // request is aborted
  ADSERR_DEVICE_WARNING		      = ($20+ERR_ADSERRS); // signal warning
  ADSERR_DEVICE_INVALIDARRAYIDX	      = ($21+ERR_ADSERRS); // invalid array index
  //
  ADSERR_CLIENT_ERROR		      = ($40+ERR_ADSERRS); // Error class < client error >
  ADSERR_CLIENT_INVALIDPARM	      = ($41+ERR_ADSERRS); // invalid parameter at service call
  ADSERR_CLIENT_LISTEMPTY	      = ($42+ERR_ADSERRS); // polling list	is empty
  ADSERR_CLIENT_VARUSED		      = ($43+ERR_ADSERRS); // var connection already in use
  ADSERR_CLIENT_DUPLINVOKEID	      = ($44+ERR_ADSERRS); // invoke id in use
  ADSERR_CLIENT_SYNCTIMEOUT	      = ($45+ERR_ADSERRS); // timeout elapsed
  ADSERR_CLIENT_W32ERROR	      = ($46+ERR_ADSERRS); // error in win32 subsystem
  ADSERR_CLIENT_TIMEOUTINVALID	      = ($47+ERR_ADSERRS); //
  ADSERR_CLIENT_PORTNOTOPEN	      = ($48+ERR_ADSERRS); // ads dll
  ADSERR_CLIENT_NOAMSADDR	      = ($49+ERR_ADSERRS); // ads dll
  ADSERR_CLIENT_SYNCINTERNAL	      = ($50+ERR_ADSERRS); // internal error in ads sync
  ADSERR_CLIENT_ADDHASH		      = ($51+ERR_ADSERRS); // hash table overflow
  ADSERR_CLIENT_REMOVEHASH	      = ($52+ERR_ADSERRS); // key not found in hash table
  ADSERR_CLIENT_NOMORESYM	      = ($53+ERR_ADSERRS); // no more symbols in cache
  ADSERR_CLIENT_SYNCRESINVALID	      = ($54+ERR_ADSERRS); // invalid response received
  ADSERR_CLIENT_SYNCPORTLOCKED	      = ($55+ERR_ADSERRS); // sync port is locked

type
  TAmsNetId=record
    b:array[0..5] of Byte;
  end;
  PAmsNetId =^TAmsNetId;

  TAmsAddr=record
    netId  :TAmsNetId;
    port   :Word;
  end;
  PAmsAddr  =^TAmsAddr;
  PPAmsAddr =^PAmsAddr;


  TAdsVersion=record
    version  :Byte;
    revision :Byte;
    build    :Word;
  end;
  PAdsVersion=^TAdsVersion;


{ nTransMode types }
const
  ADSTRANS_NOTRANS	= $00000000;
  ADSTRANS_CLIENTCYCLE  = $00000001;
  ADSTRANS_CLIENT1REQ	= $00000002;
  ADSTRANS_SERVERCYCLE  = $00000003;
  ADSTRANS_SERVERONCHA  = $00000004;

  {ads states }
  ADSSTATE_INVALID      = $00000000;
  ADSSTATE_IDLE         = $00000001;
  ADSSTATE_RESET        = $00000002;
  ADSSTATE_INIT         = $00000003;
  ADSSTATE_START        = $00000004;
  ADSSTATE_RUN          = $00000005;
  ADSSTATE_STOP         = $00000006;
  ADSSTATE_SAVECFG      = $00000007;
  ADSSTATE_LOADCFG      = $00000008;
  ADSSTATE_POWERFAILURE = $00000009;
  ADSSTATE_POWERGOOD    = $0000000A;
  ADSSTATE_ERROR        = $0000000B;
  ADSSTATE_SHUTDOWN     = $0000000C;
  ADSSTATE_SUSPEND      = $0000000D;
  ADSSTATE_RESUME       = $0000000E;
  ADSSTATE_CONFIG       = $0000000F;// system is in config mode
  ADSSTATE_RECONFIG     = $00000010;// system should restart in config mode
  ADSSTATE_MAXSTATES    = $00000011;
  ////////////////////////////////////////////////////////////////////////////////

type
  {notification attributes}
  TAdsNotificationAttrib = record
    cbLength   :Longword;
    nTransMode :Longword;  //ADSTRANSMODE
    nMaxDelay  :Longword;
    case integer of
      0:(nCycleTime     :Longword);
      1:(dwChangeFilter :Longword);
  end;
  PAdsNotificationAttrib = ^TAdsNotificationAttrib;

  {notification header}
  TAdsNotificationHeader = record
    hNotification :Longword;
    nTimeStamp    :TFileTime;  {int64}
    cbSampleSize  :Longword;
    data          :Byte; //Array[1..ANYSIZE_ARRAY] of Byte;
  end;
  PAdsNotificationHeader  = ^TAdsNotificationHeader;
  PPAdsNotificationHeader = ^PAdsNotificationHeader;

{Return pointer to first notification data byte }
function fADSNOTIFICATION_PDATA( pAdsNotificationHeader : PAdsNotificationHeader ) :Pointer;

////////////////////////////////////////////////////////////////////////////////
{notification callback prototype}
type
  TAdsNotificationFuncEx = Procedure(pAddr         :PAmsAddr;
                                     pNotification :PAdsNotificationHeader;
                                     hUser         :Longword); stdcall;
  PAdsNotificationFuncEx = ^TAdsNotificationFuncEx;


const
  ADSSYMBOLFLAG_PERSISTENT  = $00000001;
  ADSSYMBOLFLAG_BITVALUE    = $00000002;


type
  TAdsSymbolEntry = record
    entryLength   :Longword;        // length of complete symbol entry
    iGroup        :Longword;        // indexGroup of symbol: input, output etc.
    iOffs         :Longword;        // indexOffset of symbol
    size          :Longword;        // size of symbol ( in bytes, 0 = bit )
    dataType      :Longword;        // adsDataType of symbol
    flags         :Longword;        // see above
    nameLength    :Word;            // length of symbol name (excl. \0)
    typeLength    :Word;            // length of type name (excl. \0)
    commentLength :Word;            // length of comment (excl. \0)
    //name        :Array[] of Byte; // name of symbol with terminating \0
    //type        :Array[] of Byte; // type name of symbol with terminating \0
    //comment     :Array[] of Byte; // comment of symbol with terminating \0
  end;
  PAdsSymbolEntry   = ^TAdsSymbolEntry;
  PPAdsSymbolEntry  = ^PAdsSymbolEntry;

function fPADSSYMBOL_NAME(pEntry      :PAdsSymbolEntry) :PAnsiChar;
function fPADSSYMBOL_TYPE(pEntry      :PAdsSymbolEntry) :PAnsiChar;
function fPADSSYMBOL_COMMENT(pEntry   :PAdsSymbolEntry) :PAnsiChar;
function fPADSSYMBOL_NEXTENTRY(pEntry :PAdsSymbolEntry) :PAdsSymbolEntry;

const
  ADSDATATYPEFLAG_DATATYPE   = $00000001;
  ADSDATATYPEFLAG_DATAITEM   = $00000002;
  ADSDATATYPE_VERSION_NEWEST = $00000001;


type
  TAdsDatatypeArrayInfo = record
    lBound   :Longword;
    elements :Longword;
  end;
  PAdsDatatypeArrayInfo = ^TAdsDatatypeArrayInfo;


type
  TAdsDatatypeEntry = record
    entryLength    :Longword;                           // length of complete datatype entry
    version        :Longword;                           // version of datatype structure
    hashValue      :Longword;                           // hashValue of datatype to compare datatypes
    typeHashValue  :Longword;                           // hashValue of base type
    size           :Longword;                           // size of datatype ( in bytes )
    offs           :Longword;                           // offs of dataitem in parent datatype ( in bytes )
    dataType       :Longword;                           // adsDataType of symbol (if alias)
    flags          :Longword;                           //
    nameLength     :Word;                               // length of datatype name (excl. \0)
    typeLength     :Word;                               // length of dataitem type name (excl. \0)
    commentLength  :Word;                               // length of comment (excl. \0)
    arrayDim       :Word;                               //
    subItems       :Word;                               //
    //name         :Array[] of Byte;                    // name of datatype with terminating \0
    //type         :Array[] of Byte;                    // type name of dataitem with terminating \0
    //comment      :Array[] of Byte;                    // comment of datatype with terminating \0
    //array        :Array[] of TAdsDatatypeArrayInfo;   //
    //subItems     :Array[] of TAdsDatatypeEntry;       //
  end;
  PAdsDatatypeEntry = ^TAdsDatatypeEntry;
  PPAdsDatatypeEntry = ^PAdsDatatypeEntry;

function    fPADSDATATYPE_NAME( pEntry      :PAdsDatatypeEntry )                             :PAnsiChar;
function    fPADSDATATYPE_TYPE( pEntry      :PAdsDatatypeEntry )                             :PAnsiChar;
function    fPADSDATATYPE_COMMENT( pEntry   :PAdsDatatypeEntry )                             :PAnsiChar;
function    fPADSDATATYPE_ARRAYINFO( pEntry :PAdsDatatypeEntry )                             :PAdsDatatypeArrayInfo;
function    fADSDATATYPE_STRUCTITEM( pEntry :PAdsDatatypeEntry; nItem : Word{0..subItems-1} ):PAdsDatatypeEntry; // return pointer to subitem

type
  TAdsSymbolUploadInfo = record
      nSymbols  :Longword;
      nSymSize  :Longword;
  end;
  PAdsSymbolUploadInfo = ^TAdsSymbolUploadInfo;

type
  TAdsSymbolInfoByName = record
    indexGroup  :Longword;
    indexOffset :Longword;
    cbLength    :Longword;
  end;
  PAdsSymbolInfoByName = ^TAdsSymbolInfoByName;

  {router events}
const
  AMSEVENT_ROUTERSTOP    = $00000000;
  AMSEVENT_ROUTERSTART   = $00000001;
  AMSEVENT_ROUTERREMOVED = $00000002;

{router notification callback prototype}
type
  TAmsRouterNotificationFuncEx = Procedure(nEvent:Longint); stdcall;
  PAmsRouterNotificationFuncEx = ^TAmsRouterNotificationFuncEx;

{SUM commands (read/write list of variables)}
type
  TAdsSumReadReq = record
  { first item }
    indexGroup    :Longword; // index group in ADS server interface
    indexOffset   :Longword; // index offset in ADS server interface
    length        :Longword; // count of bytes to read
  { second item
    indexGroup    :Longword; // index group in ADS server interface
    indexOffset   :Longword; // index offset in ADS server interface
    length        :Longword; // count of bytes to read
  }
  { third item
    indexGroup    :Longword; // index group in ADS server interface
  }
  end;
  PAdsSumReadReq = ^TAdsSumReadReq;

type
  TAdsSumWriteReq = record
  { first item }
    indexGroup  :Longword; // index group in ADS server interface
    indexOffset :Longword; // index offset in ADS server interface
    length      :Longword; // count of bytes to write
  { second item
    indexGroup  :Longword; // index group in ADS server interface
    indexOffset :Longword; // index offset in ADS server interface
    length      :Longword; // count of bytes to write
  }
  { third item
    indexGroup  :Longword; // index group in ADS server interface
  }
  { write data of first item
    data        :Array[1..ANYSIZE_ARRAY] of Byte; }
  { write data of second item
    data        :Array[1..ANYSIZE_ARRAY] of Byte; }
  { write data of third item }
  end;
  PAdsSumWriteReq = ^TAdsSumWriteReq;

type
  TAdsSumReadWriteReq = record
  { first item }
    indexGroup  :Longword; // index group in ADS server interface
    indexOffset :Longword; // index offset in ADS server interface
    readLen     :Longword; // count of byte to read
    writeLen    :Longword; // count of bytes to write
  { second item
    indexGroup  :Longword; // index group in ADS server interface
    indexOffset :Longword; // index offset in ADS server interface
    readLen     :Longword; // count of byte to read
    writeLen    :Longword; // count of bytes to write
  }
  { third item
    indexGroup  :Longword; // index group in ADS server interface
  }
  { write data of first item
    data        :Array[1..ANYSIZE_ARRAY] of Byte;
  }
  { write data of second item
    data        :Array[1..ANYSIZE_ARRAY] of Byte;
  }
  { write data of third item}
  end;
  PAdsSumReadWriteReq = ^TAdsSumReadWriteReq;

{
function fADS_SUMUP_IGRP(pEntry : PAdsSumupEntry, nItem : Word)	: Longword;
function fADS_SUMUP_IOFFS(pEntry : PAdsSumupEntry, nItem : Word): Longword;
function fADS_SUMUP_LENGTH(pEntry : PAdsSumupEntry, nItem: Word): Longword;
 }

{$IfDef DELPHI_6_OR_NEWER}
{ ADS DATA TYPES }
type
  ADS_DATATYPE =(
  ADST_VOID     = 0,                     //VT_EMPTY;     nothing
  ADST_INT16    = 2,                     //VT_I2,        2 byte signed int
  ADST_INT32    = 3,                     //VT_I4,        4 byte signed int
  ADST_REAL32   = 4,                     //VT_R4,        4 byte real
  ADST_REAL64   = 5,                     //VT_R8,        8 byte real
  ADST_INT8     = 16,                    //VT_I1,        signed char
  ADST_UINT8    = 17,                    //VT_UI1,       unsigned char
  ADST_UINT16   = 18,                    //VT_UI2,       unsigned short
  ADST_UINT32   = 19,                    //VT_UI4,       unsigned int
  ADST_INT64    = 20,                    //VT_I8,        signed 64-bit int
  ADST_UINT64   = 21,                    //VT_UI8,       unsigned 64-bit int
  ADST_STRING   = 30,                    //VT_LPSTR,     null terminated string
  ADST_WSTRING  = 31,                    //VT_LPWSTR,    wide null terminated string
  ADST_REAL80   = ord(ADST_WSTRING) + 1, //VT_LPWSTR+1,  reserved
  ADST_BIT      = ord(ADST_WSTRING) + 2, //VT_LPWSTR+2,  boolean
  ADST_BIGTYPE  = 65                     //VT_BLOB,      data bytes
    );
{$EndIf}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
{$ALIGN ON}
implementation

////////////////////////////////////////////////////////////////////////////////
function fADSNOTIFICATION_PDATA( pAdsNotificationHeader : PAdsNotificationHeader ) : Pointer;
begin
    if pAdsNotificationHeader=Nil then
        fADSNOTIFICATION_PDATA := Nil
    else
        fADSNOTIFICATION_PDATA := @pAdsNotificationHeader^.data;
end;

////////////////////////////////////////////////////////////////////////////////
function fPADSSYMBOL_NAME( pEntry : PAdsSymbolEntry ) : PAnsiChar;
begin
    if pEntry = Nil then
        fPADSSYMBOL_NAME := Nil
    else
        fPADSSYMBOL_NAME    := Pointer(Longword(pEntry) + sizeof(TAdsSymbolEntry)); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
function fPADSSYMBOL_TYPE( pEntry : PAdsSymbolEntry ) : PAnsiChar;
begin
    if pEntry = Nil then
        fPADSSYMBOL_TYPE := Nil
    else
        fPADSSYMBOL_TYPE := Pointer(Longword(fPADSSYMBOL_NAME(pEntry)) + pEntry^.nameLength + 1); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
function fPADSSYMBOL_COMMENT( pEntry : PAdsSymbolEntry ) : PAnsiChar;
begin
    if pEntry = Nil then
        fPADSSYMBOL_COMMENT := Nil
    else
        fPADSSYMBOL_COMMENT := Pointer(Longword(fPADSSYMBOL_TYPE(pEntry)) + pEntry^.typeLength + 1); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
function fPADSSYMBOL_NEXTENTRY( pEntry : PAdsSymbolEntry ) : PAdsSymbolEntry;
var pNextLen : ^Longword;
begin
    pNextLen := Nil;
    if pEntry <> Nil then
    begin
        pNextLen :=  Pointer(Longword(pEntry) + pEntry^.entryLength); { *Преобразовано из Ptr* }
        if pNextLen^ = 0 then
            pNextLen := Nil;

    end;
    fPADSSYMBOL_NEXTENTRY := Pointer(Longword(pNextLen)); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
function	fPADSDATATYPE_NAME( pEntry : PAdsDatatypeEntry ) : PAnsiChar;
begin
    if pEntry = Nil then
        fPADSDATATYPE_NAME := Nil
    else
        fPADSDATATYPE_NAME := Pointer(Longword(pEntry) + sizeof(TAdsDatatypeEntry)); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
function	fPADSDATATYPE_TYPE( pEntry : PAdsDatatypeEntry )	: PAnsiChar;
begin
    if pEntry = Nil then
        fPADSDATATYPE_TYPE := Nil
    else
        fPADSDATATYPE_TYPE := Pointer(Longword(fPADSDATATYPE_NAME(pEntry)) + pEntry^.nameLength + 1); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
function	fPADSDATATYPE_COMMENT( pEntry : PAdsDatatypeEntry ) : PAnsiChar;
begin
    if pEntry = Nil then
        fPADSDATATYPE_COMMENT := Nil
    else
        fPADSDATATYPE_COMMENT := Pointer(Longword(fPADSDATATYPE_TYPE(pEntry)) + pEntry^.typeLength + 1); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
function  fPADSDATATYPE_ARRAYINFO( pEntry : PAdsDatatypeEntry ) : PAdsDatatypeArrayInfo;
begin
    if pEntry = Nil then
        fPADSDATATYPE_ARRAYINFO := Nil
    else
        fPADSDATATYPE_ARRAYINFO := Pointer(Longword(fPADSDATATYPE_COMMENT(pEntry)) + pEntry^.commentLength + 1); { *Преобразовано из Ptr* }
end;

////////////////////////////////////////////////////////////////////////////////
{ Return pointer to subitem }
function    fADSDATATYPE_STRUCTITEM( pEntry :PAdsDatatypeEntry; nItem : Word{0..subItems-1} ) : PAdsDatatypeEntry;
var
    i        : Word;
    pItem    : PAdsDatatypeEntry;
begin
    if ( nItem >= pEntry^.subItems ) then
        fADSDATATYPE_STRUCTITEM := Nil
    else
    begin
        pItem := Pointer(Longword(fPADSDATATYPE_ARRAYINFO(pEntry)) + (pEntry^.arrayDim * sizeof(TAdsDatatypeArrayInfo))); { *Преобразовано из Ptr* }
        for i:=0 to nItem - 1 do
        begin
            pItem := Pointer(Longword(pItem) + pItem^.entryLength); { *Преобразовано из Ptr* }
        end;

        fADSDATATYPE_STRUCTITEM := pItem;
    end;
end;

initialization
  IsMultiThread := True;
   
finalization


end.
