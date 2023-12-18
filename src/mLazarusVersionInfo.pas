Unit mLazarusVersionInfo;

//
// Code by Mike Thompson - mike.cornflake@gmail.com
// http://forum.lazarus.freepascal.org/index.php/topic,13957.msg73542.html#msg73542
//
// Modified by Domenico Mammola

{$mode objfpc}

Interface

(*
  Building on the excellent vinfo.pas supplied by Paul Ishenin and available elsewhere on the Lazarus
  Forums
    - I hid the TVersionInfo class from the end user to simplify their (mine) number of required Uses...
    - Added defensive code to TVersionInfo if no build info is compiled into the exe
    - Deduced GetResourceStrings - works under Linux 64/GTK2 with Lazarus 0.9.30, but fails under
      Win XP 32bit/Lazarus 0.9.29 - suspecting my install as the lazresexplorer example also fails
      for me under Lazarus 0.9.29, but works with Lazarus 0.9.30

  Trawled through IDE source code, FPC source code and Lazarus supplied example program lasresexplorer
  to find the other defines and lookups...

  End user only needs to use VersionSupport - no other units necessary for their project.

  Jedi CodeFormatter seems to fail on the {$I %VARIABLE%} references, so sticking them all in here
  means end user code can be neatly formatted using Jedi CodeFormatter

  Other interesting includes I picked up in my travels are...
  //  {$I %HOME%} = User Home Directory
  //  {$I %FILE%} = Current pas file
  //  {$I %LINE%} = current line number

  UPDATE:  GetCPU added

  Mike Thompson - mike.cornflake@gmail.com
  March 24 2016
*)

Uses
  Classes, SysUtils, versiontypes;

// Surfacing general defines and lookups
Function GetCompiledDate: String;
Function GetCompilerInfo: String;
Function GetTargetInfo: String;
Function GetOS: String;
Function GetCPU: String;
{$IFDEF LCL}
Function GetLCLVersion: String;
Function GetWidgetSet: String;
{$ENDIF}

// Exposing resource and version info compiled into exe
Function GetResourceStrings(oStringList : TStringList) : Boolean;
function GetFileVersionAsString(const aDigits : integer = 4): String;
function GetFileVersion: TFileProductVersion;
Function GetProductVersion: String;


function CompareFileVersion (const aVer1, aVer2: TFileProductVersion): integer;

{$IFDEF LCL}
Const
  WIDGETSET_GTK        = 'GTK widget set';
  WIDGETSET_GTK2       = 'GTK 2 widget set';
  WIDGETSET_WIN        = 'Win32/Win64 widget set';
  WIDGETSET_WINCE      = 'WinCE widget set';
  WIDGETSET_CARBON     = 'Carbon widget set';
  WIDGETSET_QT         = 'QT widget set';
  WIDGETSET_fpGUI      = 'fpGUI widget set';
  WIDGETSET_OTHER      = 'Other gui';
{$ENDIF}

Implementation

Uses
  resource, versionresource
  {$IFDEF LCL}, LCLVersion, InterfaceBase, LCLPlatformDef{$ENDIF}
  ;

{$IFDEF LCL}
function GetWidgetSet: String;
Begin
  Case WidgetSet.LCLPlatform Of
    lpGtk:   Result := WIDGETSET_GTK;
    lpGtk2:  Result := WIDGETSET_GTK2;
    lpWin32: Result := WIDGETSET_WIN;
    lpWinCE: Result := WIDGETSET_WINCE;
    lpCarbon:Result := WIDGETSET_CARBON;
    lpQT:    Result := WIDGETSET_QT;
    lpfpGUI: Result := WIDGETSET_fpGUI;
  Else
    Result:=WIDGETSET_OTHER;
  End;
End;
{$ENDIF}

function GetCompilerInfo: String;
begin
  Result := 'FPC '+{$I %FPCVERSION%};
end;

function GetTargetInfo: String;
Begin
  Result := {$I %FPCTARGETCPU%}+' - '+{$I %FPCTARGETOS%};
End;

function GetOS: String;
Begin
  Result := {$I %FPCTARGETOS%};
End;

function GetCPU: String;
begin
  Result := {$I %FPCTARGETCPU%};
end;

{$IFDEF LCL}
function GetLCLVersion: String;
Begin
  Result := 'LCL '+lcl_version;
End;
{$ENDIF}

function GetCompiledDate: String;
Var
  sDate, sTime: String;
Begin
  sDate := {$I %DATE%};
  sTime := {$I %TIME%};

  Result := sDate + ' at ' + sTime;
End;

{ Routines to expose TVersionInfo data }

Type
  TVersionInfo = Class
  private
    FBuildInfoAvailable: Boolean;
    FVersResource: TVersionResource;
    Function GetFixedInfo: TVersionFixedInfo;
    Function GetStringFileInfo: TVersionStringFileInfo;
    Function GetVarFileInfo: TVersionVarFileInfo;
  public
    Constructor Create;
    Destructor Destroy; override;

    Procedure Load(Instance: THandle);

    Property BuildInfoAvailable: Boolean Read FBuildInfoAvailable;

    Property FixedInfo: TVersionFixedInfo Read GetFixedInfo;
    Property StringFileInfo: TVersionStringFileInfo Read GetStringFileInfo;
    Property VarFileInfo: TVersionVarFileInfo Read GetVarFileInfo;
  End;

Var
  FInfo: TVersionInfo;

Procedure CreateInfo;
Begin
  If Not Assigned(FInfo) Then
  Begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  End;
End;

function GetResourceStrings(oStringList: TStringList): Boolean;
Var
  i, j : Integer;
  oTable : TVersionStringTable;
begin
  CreateInfo;

  oStringList.Clear;
  Result := False;

  If FInfo.BuildInfoAvailable Then
  Begin
    Result := True;
    For i := 0 To FInfo.StringFileInfo.Count-1 Do
    Begin
      oTable := FInfo.StringFileInfo.Items[i];

      For j := 0 To oTable.Count-1 Do
        If Trim(oTable.ValuesByIndex[j])<>'' Then
          oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    end;
  end;
end;

Function ProductVersionToString(PV: TFileProductVersion; const aDigits : integer = 4): String;
Begin
  case aDigits of
    1: Result := Format('%d', [PV[0]]);
    2: Result := Format('%d.%d', [PV[0], PV[1]]);
    3: Result := Format('%d.%d.%d', [PV[0], PV[1], PV[2]]);
    else
      Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
  end;
End;

function GetFileVersionAsString(const aDigits : integer = 4): String;
begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.FileVersion, aDigits)
  Else
    Result := 'No build information available';
end;

function GetFileVersion: TFileProductVersion;
begin
  CreateInfo;

  Result := FInfo.FixedInfo.FileVersion;
end;

function GetProductVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
  Else
    Result := 'No build information available';
End;

function CompareFileVersion(const aVer1, aVer2: TFileProductVersion): integer;
  function CompareWord (const aW1, aW2: Word): integer;
  begin
    if aW1 < aW2 then
      Result := -1
    else if aW1 > aW2 then
      Result := +1
    else
      Result := 0;
  end;
begin
  Result := CompareWord(aVer1[0], aVer2[0]);
  if Result = 0 then
    Result := CompareWord(aVer1[1], aVer2[1]);
  if Result = 0 then
    Result := CompareWord(aVer1[2], aVer2[2]);
  if Result = 0 then
    Result := CompareWord(aVer1[3], aVer2[3]);
end;


{ TVersionInfo }

Function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
Begin
  Result := FVersResource.FixedInfo;
End;

Function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
Begin
  Result := FVersResource.StringFileInfo;
End;

Function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
Begin
  Result := FVersResource.VarFileInfo;
End;

Constructor TVersionInfo.Create;
Begin
  Inherited Create;

  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
End;

Destructor TVersionInfo.Destroy;
Begin
  FVersResource.Free;

  Inherited Destroy;
End;

Procedure TVersionInfo.Load(Instance: THandle);
Var
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
Begin
  FBuildInfoAvailable := False;
  ResID := 1;

  // Defensive code to prevent failure if no resource available...
  Res := FindResource(Instance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
  If Res = 0 Then
    Exit;

  Stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
  Try
    FVersResource.SetCustomRawDataStream(Stream);

    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  Finally
    Stream.Free;
  End;
End;

Initialization
  FInfo := nil;

Finalization
  If Assigned(FInfo) Then
    FInfo.Free;
End.
