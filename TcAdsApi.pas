////////////////////////////////////////////////////////////////////////////////
//        File: TcAdsAPI.pas
// Description: Prototypes and Definitions for non C++ Applications
//      Author: ChristophC
//     Created: 25.03.2003
//
//
// BECKHOFF-Industrieelektronik-GmbH
//
// Modifications:
// 15.04.2003 ChristophC: AdsAmsRegisterRouterNotification and AdsAmsUnRegisterRouterNotification functions added
//
// 16.10.2007 ChristophC: Multithreading function prototypes added
//
// 20.02.2008 ChristophC: New pascal functions added:
// AdsSyncSumReadReq, AdsSyncSumWriteReq, AdsSyncSumReadWriteReq
//
// 31.08.2010 ChristophC:
// - IsMultiThread := True; in the initialization section added to allow the memory manager to execute safely
// if a VCL application has more than one thread. If a second thread attempts to allocate memory,
// it is blocked until the first thread exits the memory manager. Delphi is thread-safe, but only when the developer tells it to be.
// - AdsSyncSumReadReqEx() optimizations/fixes
//
// 07.09.2010 ChristophC: String/AnsiString/WideString/Char/PChar/AnsiChar/WideChar compatibility problem in newer Delphi version solved
//
////////////////////////////////////////////////////////////////////////////////

{$ALIGN OFF}      {Disable aligned records}
unit TcAdsAPI;

interface
uses sysutils,windows, TcAdsDef;
{TcAdsDLL Prototypes}
function AdsGetDllVersion():Longint; stdcall; external 'TcAdsDll.dll' name '_AdsGetDllVersion@0';

function AdsPortClose():Longint;stdcall; external 'TcAdsDll.dll' name '_AdsPortClose@0';

function AdsPortOpen():Longint; stdcall; external 'TcAdsDll.dll' name '_AdsPortOpen@0';

function AdsGetLocalAddress( pAddr:PAmsAddr ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsGetLocalAddress@4';

function AdsSyncWriteReq(    pAddr:PAmsAddr;
                             indexGroup, indexOffset, length :Longword;
                             pData:Pointer ):Longint; stdcall; external 'TcAdsDll.dll' name  '_AdsSyncWriteReq@20';

function AdsSyncReadReq(     pAddr:PAmsAddr;
                             indexGroup, indexOffset, length:Longword;
                             pData:Pointer ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadReq@20';

function AdsSyncReadReqEx(   pAddr:PAmsAddr;
                             indexGroup, indexOffset, length:Longword;
                             pData:Pointer;
                             pcbReturn :PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadReqEx@24';


function AdsSyncReadWriteReq(  pAddr:PAmsAddr;
                               indexGroup, indexOffset, cbReadLength:Longword;
                               pReadData:Pointer;
                               cbWriteLength:Longword;
                               pWriteData:Pointer ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadWriteReq@28';

function AdsSyncReadWriteReqEx( pAddr:PAmsAddr;
                                indexGroup, indexOffset, cbReadLength:Longword;
                                pReadData:Pointer;
                                cbWriteLength:Longword;
                                pWriteData:Pointer;
                                pcbReturn :PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadWriteReqEx@32';


function AdsSyncReadDeviceInfoReq( pAddr:PAmsAddr;
                                   pDevName:PAnsiChar;
                                   pVersion:PAdsVersion ):Longint;stdcall; external 'TcAdsDll.dll' name  '_AdsSyncReadDeviceInfoReq@12';

function AdsSyncWriteControlReq( pAddr:PAmsAddr;
                                 nAdsState, nDeviceState:Word;
                                 nLength:Longword;
                                 pData:Pointer ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncWriteControlReq@20';

function AdsSyncReadStateReq( pAddr:PAmsAddr;
                              pAdsState, pDeviceState:PWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadStateReq@12';

function AdsSyncAddDeviceNotificationReq( pAddr:PAmsAddr;
                                          indexGroup, indexOffset:Longword;
                                          pNoteAttrib:PAdsNotificationAttrib;
                                          pNoteFunc:PAdsNotificationFuncEx;
                                          hUser:Longword;
                                          pNotification:PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncAddDeviceNotificationReq@28';

function AdsSyncDelDeviceNotificationReq( pAddr:PAmsAddr;
                                          hNotification:Longword ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncDelDeviceNotificationReq@8';

function AdsSyncSetTimeout( nMs:Longint ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncSetTimeout@4';




function AdsAmsRegisterRouterNotification(  pNoteFunc: PAmsRouterNotificationFuncEx ):Longint;  stdcall; external 'TcAdsDll.dll' name '_AdsAmsRegisterRouterNotification@4';

function AdsAmsUnRegisterRouterNotification ():Longint; stdcall; external 'TcAdsDll.dll' name '_AdsAmsUnRegisterRouterNotification@0';

{

function AdsGetLastError( ): Longint; stdcall; external 'TcAdsDll.dll' name '_AdsGetLastError@0';

function AdsSyncGetTimeout( pMs:PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncGetTimeout@4';

function AdsAmsPortEnabled( pEnabled : PLONGBOOL ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsAmsPortEnabled@4';

}



////////////////////////////////////////////////////////////////////////////////////////////////////
// new Ads functions for multithreading applications
////////////////////////////////////////////////////////////////////////////////////////////////////

function AdsPortOpenEx():Longint; stdcall; external 'TcAdsDll.dll' name '_AdsPortOpenEx@0';

function AdsPortCloseEx( port : Longint  ):Longint;stdcall; external 'TcAdsDll.dll' name '_AdsPortCloseEx@4';

function AdsGetLocalAddressEx( port : Longint; pAddr:PAmsAddr ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsGetLocalAddressEx@8';

function AdsSyncWriteReqEx(    port : Longint;
			 	pAddr:PAmsAddr;
                             indexGroup, indexOffset, length :Longword;
                             pData:Pointer ):Longint; stdcall; external 'TcAdsDll.dll' name  '_AdsSyncWriteReqEx@24';

function AdsSyncReadReqEx2(     port : Longint;
				pAddr:PAmsAddr;
                             indexGroup, indexOffset, length:Longword;
                             pData:Pointer;
                             pcbReturn :PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadReqEx2@28';

function AdsSyncReadWriteReqEx2( port :Longint; 
				pAddr:PAmsAddr;
                                indexGroup, indexOffset, cbReadLength:Longword;
                                pReadData:Pointer;
                                cbWriteLength:Longword;
                                pWriteData:Pointer;
                                pcbReturn :PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadWriteReqEx2@36';

function AdsSyncReadDeviceInfoReqEx( port : Longint;
					pAddr:PAmsAddr;
                                   pDevName:PAnsiChar;
                                   pVersion:PAdsVersion ):Longint;stdcall; external 'TcAdsDll.dll' name  '_AdsSyncReadDeviceInfoReqEx@16';

function AdsSyncWriteControlReqEx( port : Longint;
				pAddr:PAmsAddr;
                                 nAdsState, nDeviceState:Word;
                                 nLength:Longword;
                                 pData:Pointer ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncWriteControlReqEx@24';

function AdsSyncReadStateReqEx( port : Longint;
				pAddr:PAmsAddr;
                              pAdsState, pDeviceState:PWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncReadStateReqEx@16';

function AdsSyncAddDeviceNotificationReqEx( port : Longint;
                                          pAddr:PAmsAddr;
                                          indexGroup, indexOffset:Longword;
                                          pNoteAttrib:PAdsNotificationAttrib;
                                          pNoteFunc:PAdsNotificationFuncEx;
                                          hUser:Longword;
                                          pNotification:PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncAddDeviceNotificationReqEx@32';

function AdsSyncDelDeviceNotificationReqEx( port : Longint;
						pAddr:PAmsAddr;
            hNotification:Longword ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncDelDeviceNotificationReqEx@12';


function AdsSyncSetTimeoutEx( port : Longint;
				nMs:Longint ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncSetTimeoutEx@8';


function AdsSyncGetTimeoutEx( port : Longint;
				pMs:PLONGWORD ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsSyncGetTimeoutEx@8';


function AdsAmsPortEnabledEx( port : Longint;
				pEnabled : PLONGBOOL ):Longint; stdcall; external 'TcAdsDll.dll' name '_AdsAmsPortEnabledEx@8';




////////////////////////////////////////////////////////////////////////////////
// new (DELPHI only) ADS READ/WRITE list commands
////////////////////////////////////////////////////////////////////////////////
function AdsSyncSumReadReq( pAddr : PAmsAddr;
                            indexGroup, indexOffset, readLength : Array of Longword;
                            readPtr : Array of Pointer;
                            var results : Array of Longint ) : Longint;

function AdsSyncSumReadReqEx(   pAddr : PAmsAddr;
                                indexGroup, indexOffset, readLength : Array of Longword;
                                readPtr : Array of Pointer;
                                var results : Array of Longint;
                                var retLength : Array of Longword ) : Longint;

function AdsSyncSumWriteReq( pAddr : PAmsAddr;
                            indexGroup, indexOffset, writeLength : Array of Longword;
                            writePtr : Array of Pointer;
                            var results : Array of Longint ) : Longint;

function AdsSyncSumReadWriteReq( pAddr : PAmsAddr;
                                indexGroup, indexOffset: Array of Longword;
                                readLength  : Array of Longword;
                                readPtr  : Array of Pointer;
                                writeLength : Array of Longword;
                                writePtr   : Array of Pointer;
                                var results : Array of Longint;
                                var retLength : Array of Longword ) : Longint;



{$ALIGN ON}
implementation

//////////////////////////////////////////////////////////////////////////////
// Send list of ADS READ commands
//////////////////////////////////////////////////////////////////////////////
function AdsSyncSumReadReq( pAddr : PAmsAddr;
                            indexGroup, indexOffset, readLength : Array of Longword;
                            readPtr : Array of Pointer;
                            var results : Array of Longint ) : Longint;
var retLength : Array of Longword;
begin
    SetLength(retLength, Length(indexGRoup));
    AdsSyncSumReadReq := AdsSyncSumReadReqEx(pAddr,
                                indexGroup, indexOffset,
                                readLength, readPtr,
                                results, retLength );
end;


//////////////////////////////////////////////////////////////////////////////
// Send list of EXTENDED ADS READ commands
//////////////////////////////////////////////////////////////////////////////
function AdsSyncSumReadReqEx(   pAddr : PAmsAddr; indexGroup, indexOffset, readLength : Array of Longword;
                                readPtr : Array of Pointer;
                                var results : Array of Longint;
                                var retLength : Array of Longword ) : Longint;
var read, write : Array of Byte;
    cbRead, cbWrite, nElems, dataOffset, cbCopy, cbReturned : Longword;
    i, adsErr : Longint;
begin
    adsErr := ADSERR_DEVICE_INVALIDPARM;

    if (Length(indexGroup) = Length(indexOffset))
        And (Length(indexGroup) = Length(readLength))
        And (Length(indexGroup) = Length(readPtr))
        And (Length(indexGroup) = Length(results))
        And (Length(indexGroup) = Length(retLength))
        And (Length(indexGroup) > 0) then
    begin
        nElems := Length(indexGroup);// number of list elements

        // calculate the length of read/write data buffer
        cbWrite := nElems * 12;// 12 = byte size of indexGroup, indexOffset, length parameter
        cbRead  := nElems * 8;// 8 = byte size of results and retLength
        for i:=Low(readLength) to High(readLength) do
            cbRead := cbRead + readLength[i];

        SetLength(write, cbWrite);//resize write buffer
        SetLength(read, cbRead);//resize read buffer

        for i:=Low(readLength) to High(readLength) do
        begin
            Move(indexGroup[i], write[(i*12) + 0], SizeOf(indexGroup[i]));
            Move(indexOffset[i], write[(i*12) + 4], SizeOf(indexOffset[i]));
            Move(readLength[i], write[(i*12) + 8], SizeOf(readLength[i]));
        end;

        adsErr := AdsSyncReadWriteReqEx(pAddr,
                                        ADSIGRP_SUMUP_READEX,
                                        nElems,
                                        cbRead,
                                        @read[0],
                                        cbWrite,
                                        @write[0],
                                        @cbReturned );
        if adsErr = 0 then
        begin
            dataOffset := nElems * 8;
            for i:=Low(results) to High(results) do
            begin
                Move(read[i*8], results[i], SizeOf(results[i]));// get result code
                Move(read[(i*8) + 4], retLength[i], SizeOf(retLength[i]));// get read length
                if results[i] = 0 then // no error
                begin
                    if retLength[i] < readLength[i] then
                        cbCopy := retLength[i]
                    else
                        cbCopy := readLength[i];

                    if (cbCopy > 0) And (readPtr[i] <> Nil) then
                        Move(read[dataOffset], (PBYTE(readPtr[i]))^, cbCopy); // copy data

                    dataOffset := dataOffset + readLength[i];// increment to the next data (the next data element is behind the requested data length!)
                    if (dataOffset > cbRead) then
                    begin
                       adsErr := ADSERR_DEVICE_INVALIDSIZE;
                       break;
                    end;

                end;
            end;
        end;
    end;

    AdsSyncSumReadReqEx := adsErr;
end;

//////////////////////////////////////////////////////////////////////////////
// Send list of ADS WRITE commands
//////////////////////////////////////////////////////////////////////////////
function AdsSyncSumWriteReq( pAddr : PAmsAddr; indexGroup, indexOffset, writeLength : Array of Longword;
                            writePtr : Array of Pointer;
                            var results : Array of Longint ) : Longint;
var read, write : Array of Byte;
    cbRead, cbWrite, nElems, dataOffset, cbReturned : Longword;
    i, adsErr : Longint;
begin
    adsErr := ADSERR_DEVICE_INVALIDPARM;

    if (Length(indexGroup) = Length(indexOffset))
        And (Length(indexGroup) = Length(writeLength))
        And (Length(indexGroup) = Length(writePtr))
        And (Length(indexGroup) = Length(results))
        AND (Length(indexGroup) > 0) then
    begin
        nElems := Length(indexGroup);// number of list elements

        // calculate the length of read/write data buffer
        cbWrite := nElems * 12;// 12 = byte size of indexGroup, indexOffset, length parameter
        cbRead  := nElems * 4;// 4 = byte size of results
        for i:=Low(writeLength) to High(writeLength) do
            cbWrite := cbWrite + writeLength[i];

        SetLength(write, cbWrite);//resize write buffer
        SetLength(read, cbRead);//resize read buffer

        dataOffset := nElems * 12;// 12 = byte size of indexGroup, indexOffset, length parameter
        for i:=Low(writeLength) to High(writeLength) do
        begin
            Move(indexGroup[i], write[(i*12) + 0], SizeOf(indexGroup[i]));
            Move(indexOffset[i], write[(i*12) + 4], SizeOf(indexOffset[i]));
            Move(writeLength[i], write[(i*12) + 8], SizeOf(writeLength[i]));
            if (writeLength[i] <> 0) and (writePtr[i] <> Nil) then
            begin
                Move( (PBYTE(writePtr[i]))^, write[dataOffset], writeLength[i] );
                dataOffset := dataOffset + writeLength[i];
            end;
        end;

        adsErr := AdsSyncReadWriteReqEx(  pAddr,
                                        ADSIGRP_SUMUP_WRITE,
                                        nElems,
                                        cbRead,
                                        @read[0],
                                        cbWrite,
                                        @write[0],
                                        @cbReturned );
        if adsErr = 0 then
        begin
            for i:=Low(results) to High(results) do
                Move(read[i*4], results[i], SizeOf(results[i]));// get result code
        end;
    end;
    AdsSyncSumWriteReq := adsErr;
end;

//////////////////////////////////////////////////////////////////////////////
// Send list of ADS READWRITE commands
//////////////////////////////////////////////////////////////////////////////
function AdsSyncSumReadWriteReq( pAddr : PAmsAddr;
                                 indexGroup, indexOffset: Array of Longword;
                                 readLength : Array of Longword;
                                 readPtr : Array of Pointer;
                                 writeLength : Array of Longword;
                                 writePtr : Array of Pointer;
                                 var results : Array of Longint;
                                 var retLength : Array of Longword ) : Longint;
var read, write : Array of Byte;
    cbRead, cbWrite, nElems, dataOffset, cbReturned, cbCopy : Longword;
    i, adsErr : Longint;
begin
    adsErr := ADSERR_DEVICE_INVALIDPARM;

    if (Length(indexGroup) = Length(indexOffset))
        And (Length(indexGroup) = Length(readLength))
        And (Length(indexGroup) = Length(readPtr))
        And (Length(indexGroup) = Length(writeLength))
        And (Length(indexGroup) = Length(writePtr))
        And (Length(indexGroup) = Length(results))
        And (Length(indexGroup) = Length(retLength))
        And (Length(indexGroup) > 0) then
    begin
        nElems := Length(indexGroup);// number of list elements

        // calculate the length write data buffer
        cbWrite := nElems * 16;// 16 = byte size of indexGroup, indexOffset, readLength and writeLength parameter
        for i:=Low(writeLength) to High(writeLength) do
            cbWrite := cbWrite + writeLength[i];

        // calculate the length read data buffer
        cbRead  := nElems * 8;// 8 = byte size of results and readLengths
        for i:=Low(readLength) to High(readLength) do
            cbRead := cbRead + readLength[i];

        SetLength(write, cbWrite);//resize write buffer
        SetLength(read, cbRead);//resize read buffer

        dataOffset := nElems * 16;// 16 = byte size of indexGroup, indexOffset, readLength and writeLength parameter
        for i:=Low(writeLength) to High(writeLength) do
        begin
            Move(indexGroup[i], write[(i*16) + 0], SizeOf(indexGroup[i]));
            Move(indexOffset[i], write[(i*16) + 4], SizeOf(indexOffset[i]));
            Move(readLength[i], write[(i*16) + 8], SizeOf(readLength[i]));
            Move(writeLength[i], write[(i*16) + 12], SizeOf(writeLength[i]));
            if (writeLength[i] <> 0) and (writePtr[i] <> Nil) then
            begin
                Move( (PBYTE(writePtr[i]))^, write[dataOffset], writeLength[i] );
                dataOffset := dataOffset + writeLength[i];
            end;
        end;

        adsErr := AdsSyncReadWriteReqEx(  pAddr,
                                        ADSIGRP_SUMUP_READWRITE,
                                        nElems,
                                        cbRead,
                                        @read[0],
                                        cbWrite,
                                        @write[0],
                                        @cbReturned );
        if adsErr = 0 then
        begin
            dataOffset := nElems * 8;
            for i:=Low(results) to High(results) do
            begin
                Move(read[(i*8) + 0], results[i], SizeOf(results[i]));// get result code
                Move(read[(i*8) + 4], retLength[i], SizeOf(retLength[i]));// get result code
                if results[i] = 0 then // success
                begin
                    if retLength[i] < readLength[i] then
                        cbCopy := retLength[i]
                    else
                        cbCopy := readLength[i];

                    if (cbCopy > 0) And (readPtr[i] <> Nil) then
                        Move(read[dataOffset], (PBYTE(readPtr[i]))^, cbCopy); // copy data
                end;
                
                dataOffset := dataOffset + retLength[i];// increment to the next data
            end;
        end;
    end;
    AdsSyncSumReadWriteReq := adsErr;
end;





initialization
    IsMultiThread := True;
finalization

end.

