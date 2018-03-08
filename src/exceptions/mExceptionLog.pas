// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mExceptionLog;

interface

uses
  Classes, SysUtils;

type

  { TExceptionLogConfiguration }

  TExceptionLogConfiguration = class
  strict private
    FSendTraceLogByMail : boolean;
    FTraceLogMailDestination : string;
  public
    constructor Create;

    property SendTraceLogByMail : boolean read FSendTraceLogByMail write FSendTraceLogByMail;
    property TraceLogMailDestination : string read FTraceLogMailDestination write FTraceLogMailDestination;
  end;

{$IFDEF FPC}
// credits: procedure DumpExceptionCallStack(E: Exception) in http://wiki.freepascal.org/Logging_exceptions
// GetSystemMem by ChrisF in http://forum.lazarus.freepascal.org/index.php?topic=30855.0
procedure DumpExceptionCallStack(Sender: TObject; E: Exception; var aWantsToShutDown : boolean);
{$ENDIF}


function ExceptionLogConfiguration : TExceptionLogConfiguration;

implementation

uses
  {$IFDEF WINDOWS}windows,{$ENDIF} Dos,
  mExceptionLogForm,
  mUtility, mLazarusVersionInfo, mThreadsBaseClasses;

{$IFDEF WINDOWS}
type
  MEMORYSTATUSEX = record
     dwLength : DWORD;
     dwMemoryLoad : DWORD;
     ullTotalPhys : uint64;
     ullAvailPhys : uint64;
     ullTotalPageFile : uint64;
     ullAvailPageFile : uint64;
     ullTotalVirtual : uint64;
     ullAvailVirtual : uint64;
     ullAvailExtendedVirtual : uint64;
  end;

var
  _ExceptionLogConfiguration : TExceptionLogConfiguration;

function GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX): BOOL; stdcall; external 'kernel32' name 'GlobalMemoryStatusEx';

function GetSystemMem: string;  { Returns installed RAM (as viewed by your OS) in Gb\Tb}
var
  MS_Ex : MemoryStatusEx;
begin
  FillChar (MS_Ex, SizeOf(MemoryStatusEx), #0);
  MS_Ex.dwLength := SizeOf(MemoryStatusEx);
  GlobalMemoryStatusEx(MS_Ex);
  Result:= 'Total memory: ' + BytesToHumanReadableString(MS_Ex.ullTotalPhys);
  Result:= Result + sLineBreak + 'Available memory: ' + BytesToHumanReadableString(MS_Ex.ullAvailPhys);
  Result:= Result + sLineBreak + 'Free memory: ' + BytesToHumanReadableString(MS_Ex.ullAvailVirtual);
end;
{$ELSE}
function GetSystemMem: string;
begin
 Result := '?';
end;
{$ENDIF}


function GetHardwareInfo: string;
begin
  Result := GetSystemMem;
  Result := Result + sLineBreak + 'Number of cores: ' + IntToStr(GetCPUCores);
  Result := Result + sLineBreak + 'Disk size: ' + BytesToHumanReadableString(DiskSize(0));
  Result := Result + sLineBreak + 'Free disk space: ' + BytesToHumanReadableString(DiskFree(0));
end;

function GetSoftwareInfo: string;
begin
  Result := 'Application name: ' + ApplicationName;
  Result := Result + sLineBreak + 'Vendor name: ' + VendorName;
  Result := Result + sLineBreak + 'Product version: ' + GetProductVersion;
  Result := Result + sLineBreak + 'File version: ' + GetFileVersionAsString;
  Result := Result + sLineBreak + 'Target OS: ' +  GetOS;
  Result := Result + sLineBreak + 'Target CPU: ' + GetCPU;
  Result := Result + sLineBreak + 'Date of compilation: ' + GetCompiledDate;
  Result := Result + sLineBreak + 'Compiler info: ' + GetCompilerInfo;
end;

function GetSystemInfo: String;
begin
  Result := 'Current date/time: '+ DateTimeToStr(Now);
  Result := Result + sLineBreak + 'Current user: ' + GetOSUser;
end;

function GetSenderInfo(const aSender: TObject): String;
begin
  if not Assigned (aSender) then
    Result := 'Sender object: not assigned'
  else
    Result := 'Sender class name: ' + aSender.ClassName;
  if aSender is TThread then
    Result := Result + sLineBreak + 'Thread id: ' + IntToStr((aSender as TThread).Handle);
  if aSender is TmThread then
    Result := Result + sLineBreak + 'Thread debug name: ' + ((aSender as TThread) as TmThread).GetDebugInfo;
end;

function GetStackTrace: String;
var
  I: Integer;
  Frames: PPointer;
begin
  Result := BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Result := Result + sLineBreak + BackTraceStrFunc(Frames[I]);
end;

function GetExceptionInfo (e: Exception): String;
begin
  if E <> nil then begin
    Result := 'Exception class: ' + E.ClassName;
    Result := Result + sLineBreak + 'Message: ' + E.Message;
  end;
end;

function BuildTitle (const aTitle: string): String;
var
  s: string;
begin
  s := StringOfChar('-', Length(aTitle));
  Result := s + sLineBreak + aTitle + sLineBreak + s;
end;

procedure DumpExceptionCallStack(Sender: TObject; E: Exception; var aWantsToShutDown : boolean);
var
  Report: string;
  Dlg : TExceptionLogForm;
begin
  aWantsToShutDown:= false;

  Report := 'An error has occurred!';

  Report := Report + sLineBreak;
  Report := Report + sLineBreak + BuildTitle('ERROR INFO');
  Report := Report + sLineBreak + GetExceptionInfo(e);

  Report := Report + sLineBreak;
  Report := Report + sLineBreak + BuildTitle('SOFTWARE INFO');
  Report := Report + sLineBreak + GetSoftwareInfo();

  Report := Report + sLineBreak;
  Report := Report + sLineBreak + BuildTitle('SYSTEM INFO');
  Report := Report + sLineBreak + GetSystemInfo();

  Report := Report + sLineBreak;
  Report := Report + sLineBreak + BuildTitle('HARDWARE INFO');
  Report := Report + sLineBreak + GetHardwareInfo();

  Report := Report + sLineBreak;
  Report := Report + sLineBreak + BuildTitle('SENDER INFO');
  Report := Report + sLineBreak + GetSenderInfo(Sender);

  Report := Report + sLineBreak;
  Report := Report + sLineBreak + BuildTitle('STACK TRACE LOG');
  Report := Report + sLineBreak + GetStackTrace;

  Dlg := TExceptionLogForm.Create(nil);
  try
    Dlg.Init(Report);
    Dlg.ShowModal;
    aWantsToShutDown:= Dlg.UserWantsToShutDown;
  finally
    Dlg.Free;
  end;
end;

function ExceptionLogConfiguration: TExceptionLogConfiguration;
begin
  if not Assigned(_ExceptionLogConfiguration) then
    _ExceptionLogConfiguration := TExceptionLogConfiguration.Create;
  Result := _ExceptionLogConfiguration;
end;

{ TExceptionLogConfiguration }

constructor TExceptionLogConfiguration.Create;
begin
  FSendTraceLogByMail := false;
  FTraceLogMailDestination := '';
end;

finalization
  FreeAndNil(_ExceptionLogConfiguration);

end.
