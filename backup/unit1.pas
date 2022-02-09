unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Messages, Windows, Graphics, Dialogs,
  ComCtrls, StdCtrls, AdvLed, TcAdsAPI, TcAdsDef;

const WM_ADSDEVICENOTIFICATION = WM_APP + 401;

type

  { TForm1 }

  TForm1 = class(TForm)
    AdvLed1: TAdvLed;
    Label1: TLabel;
    StatusBar1: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WMAdsDeviceNotification( var Message: TMessage ); message WM_ADSDEVICENOTIFICATION;
  private
    LocalAddr     :TAmsAddr;
    ServerAddr    :TAmsAddr;
    hNotification :DWORD;
  public

  end;


{$ALIGN OFF}
type TThreadListItem = record
  netId   : TAmsNetId;
  port    : Word;
  hNotify : Longword;
  stamp   : _FILETIME;  {int64}
  cbSize  : Longword;
  hUser   : Longword;
  data    : Byte;    //Array[1..ANYSIZE_ARRAY] of Byte;
end;
PThreadListItem = ^TThreadListItem;
{$ALIGN ON}

var
  Form1         : TForm1;
  wndHandle     : HWND;
  DevThreadList : TThreadList;

implementation

{$R *.lfm}
//////////////////////////////////////////////////////////////////////////////
procedure AdsDeviceCallback(pAddr:PAmsAddr; pNotification:PAdsNotificationHeader; hUser:Longword); stdcall;
  var pItem : PThreadListItem;

begin
  pItem := Nil;
  try
    GetMem(pItem, sizeof(TThreadListItem) + pNotification^.cbSampleSize - 1 );
    pItem^.netId   := pAddr^.netId;
    pItem^.port    := pAddr^.port;
    pItem^.hNotify := pNotification^.hNotification;
    pItem^.stamp   := pNotification^.nTimeStamp;
    pItem^.cbSize  := pNotification^.cbSampleSize;
    pItem^.hUser   := hUser;
    //copy the notification data
    Move( pNotification^.data, pItem^.data, pNotification^.cbSampleSize);
  finally
    with DevThreadList.LockList do
    try
      Add( pItem );
    finally
      DevThreadList.UnlockList;
      PostMessage(wndHandle, WM_ADSDEVICENOTIFICATION, 0, 0);
    end;
  end;
end;

procedure TForm1.WMAdsDeviceNotification( var Message: TMessage );
var pItem                   : PThreadListItem;
    X                       : integer;
    FileTime                : _FILETIME;
    SystemTime, LocalTime   : _SYSTEMTIME;
    TimeZoneInformation     : _TIME_ZONE_INFORMATION;
    DateTime                : TDateTime;
    adsState                : Smallint;
    sState                  : String;
    cbData                  : Longword;
begin
    with DevThreadList.LockList do
    try
       for X := 0 to Count-1 do
       begin
           pItem := Items[X];
           {convert file time to local system time}
           FileTime:=pItem^.stamp;
           FileTimeToSystemTime(FileTime, SystemTime);
           GetTimeZoneInformation(TimeZoneInformation);
           SystemTimeToTzSpecificLocalTime(@TimeZoneInformation, SystemTime, LocalTime);
           DateTime:=SystemTimeToDateTime(LocalTime);

           cbData :=Min(pItem^.cbSize, sizeof(adsState));
           System.Move( pItem^.data, adsState, cbData );
           case adsState of
                ADSSTATE_STOP:  begin
                                     sState := 'STOP';
                                     AdvLed1.Kind := lkRedLight;
                                     AdvLed1.Hint := 'PLC is stopped';
                end;

                ADSSTATE_RUN:   begin
                                     sState := 'RUN';
                                     AdvLed1.Kind := lkGreenLight;
                                     AdvLed1.Hint := 'PLC is running';
                end;
           else
                sState := Format('Other: %d', [adsState]);
           end;

           Label1.Caption := Format( '%s %d.%d.%d.%d.%d.%d[%d], hNot:0x%x, size:%d, hUser:0x%x, State:%s',
                                     [TimeToStr(DateTime), pItem^.netId.b[0], pItem^.netId.b[1], pItem^.netId.b[2],
                                     pItem^.netId.b[3], pItem^.netId.b[4], pItem^.netId.b[5], pItem^.port,
                                     pItem^.hNotify, pItem^.cbSize, pItem^.hUser,
                                     sState ]);
           FreeMem( pItem, sizeof(TThreadListItem) + pItem^.cbSize - 1 ); //free item memory
       end;
       Clear();
    finally
       DevThreadList.UnlockList;
    end;
    inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AdsResult             :Longint;
  ClientPort            :Word;
  huser                 :Longword;
  adsNotificationAttrib :TadsNotificationAttrib;

begin
  StatusBar1.SimplePanel               := true;
  wndHandle                            := Handle;
  DevThreadList                        := TThreadList.Create();
  adsNotificationAttrib.cbLength       := sizeof(DWORD);
  adsNotificationAttrib.nTransMode     := ADSTRANS_SERVERONCHA;
  adsNotificationAttrib.nMaxDelay      := 0;
  adsNotificationAttrib.nCycleTime     := 0;
  ClientPort:= AdsPortOpen();
  if ClientPort = 0 then {error!}
    ShowMessage( 'AdsPortOpen() error!' )
  else  {OK}
  begin
    AdsResult:=AdsGetLocalAddress( @LocalAddr );
    if AdsResult = 0 then {OK}
    begin
      ServerAddr.netId:=LocalAddr.netId;{connect to the PLC on the local PC}
      ServerAddr.port:=801; {first RTS}

      StatusBar1.SimpleText:=Format('Client NetId:%d.%d.%d.%d.%d.%d  Client Port:%d,  Server Port:%d',[
              LocalAddr.netId.b[0], LocalAddr.netId.b[1], LocalAddr.netId.b[2],
              LocalAddr.netId.b[3], LocalAddr.netId.b[4], LocalAddr.netId.b[5],
              ClientPort, ServerAddr.port]);
    end
    else
      ShowMessageFmt('AdsGetLocalAddress() error:%d', [AdsResult]);


    hUser := 7;
    AdsResult:=AdsSyncAddDeviceNotificationReq( @ServerAddr,
                      ADSIGRP_DEVICE_DATA,
                      ADSIOFFS_DEVDATA_ADSSTATE,
                      @adsNotificationAttrib,
                      pointer(@AdsDeviceCallback), hUser, @hNotification  );
    if AdsResult > 0 then
      ShowMessageFmt('AdsSyncAddDeviceNotificationReq() error:%d', [AdsResult]);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  AdsResult:longint;
  X : integer;

begin
  With DevThreadList.LockList do
  try
     for X := 0 to Count-1 do
         FreeMem( Items[X], sizeof(TThreadListItem) + PThreadListItem(Items[X])^.cbSize - 1 );
     Clear();
  finally
     DevThreadList.UnlockList;
     DevThreadList.Destroy;
  end;

  AdsResult := AdsPortClose();
  if AdsResult > 0 then
    ShowMessageFmt('AdsPortClose() error:%d', [AdsResult]);
end;
end.

