// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// To enable database access throw the SDAC components, you must purchase a copy of
// the SDAC components library from Devart here:
// https://www.devart.com/sdac/

unit mDatabaseConnectionImplOnSdac;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,

  MSAccess, MSClasses,

  mDatabaseConnectionClasses,
  mDatabaseConnectionImpl;

type

  { TSdacDatabaseConnectionImpl }

  TSdacDatabaseConnectionImpl = class(TmDatabaseConnectionImpl)
  private
    FConnection : TMSConnection;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;
    function GetName : String; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    class function GetImplementationName : String;
  end;

  { TSdacDatabaseQueryImpl }

  TSdacDatabaseQueryImpl = class (TmDatabaseQueryImpl)
  private
    FConnectionImpl : TSdacDatabaseConnectionImpl;
    FQuery: TMSQuery;
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); override;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetSQL (aValue : TStringList); override;
    function SameSQL (aValue : TStringList): boolean; override;
    procedure SetUnidirectional(const aValue : boolean); override;

    procedure Open; override;
    procedure Close; override;
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure First; override;
    procedure Next; override;
    function Eof : boolean; override;
    function AsDataset : TDataset; override;
    //function ParamByName(const Value: string): TParam; override;
    function ParamCount : integer; override;
    procedure SetParamValue(aParam : TmQueryParameter); override;
    function GetParam (aIndex : integer) : TParam; override;
    function Prepared : boolean; override;
  end;

  { TSdacDatabaseCommandImpl }

  TSdacDatabaseCommandImpl = class (TmDatabaseCommandImpl)
  private
    FConnectionImpl : TSdacDatabaseConnectionImpl;
    FCommand : TMSSQL;
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); override;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetSQL (aValue : TStringList); override;
    function SameSQL (aValue : TStringList): boolean; override;

    function Execute : integer; override;

    procedure Prepare; override;
    procedure Unprepare; override;
//    function ParamByName(const Value: string): TParam; override;
    function ParamCount : integer; override;
    procedure SetParamValue(aParam : TmQueryParameter); override;
    function GetParam (aIndex : integer) : TParam; override;

    function Prepared : boolean; override;
  end;



implementation

uses
  mDatabaseConnectionImplRegister,
  SysUtils {$IFDEF WINDOWS},ActiveX{$ENDIF};

{ TSdacDatabaseCommandImpl }

procedure TSdacDatabaseCommandImpl.SetDatabaseConnectionImpl(value: TmDatabaseConnectionImpl);
begin
  FConnectionImpl := value as TSdacDatabaseConnectionImpl;
  FCommand.Connection := FConnectionImpl.FConnection;
end;

function TSdacDatabaseCommandImpl.GetDatabaseConnectionImpl: TmDatabaseConnectionImpl;
begin
  Result := FConnectionImpl;
end;

constructor TSdacDatabaseCommandImpl.Create;
begin
  FCommand := TMSSQL.Create(nil);
end;

destructor TSdacDatabaseCommandImpl.Destroy;
begin
  FCommand.Free;
  inherited Destroy;
end;

procedure TSdacDatabaseCommandImpl.SetSQL(aValue: TStringList);
begin
  FCommand.SQL.Clear;
  FCommand.SQL.AddStrings(aValue);
end;

function TSdacDatabaseCommandImpl.SameSQL(aValue: TStringList): boolean;
begin
  Result := FCommand.SQL.Count <> aValue.Count;
  if (not Result) then
    Result := (CompareStr(FCommand.SQL.Text, aValue.Text) = 0);
end;

function TSdacDatabaseCommandImpl.Execute : integer;
begin
  FCommand.Execute();
  Result := FCommand.RowsAffected;
end;

procedure TSdacDatabaseCommandImpl.Prepare;
begin
  FCommand.Prepare;
end;

procedure TSdacDatabaseCommandImpl.Unprepare;
begin
  FCommand.Unprepare;
end;

function TSdacDatabaseCommandImpl.ParamCount: integer;
begin
  Result := FCommand.ParamCount;
end;

procedure TSdacDatabaseCommandImpl.SetParamValue(aParam: TmQueryParameter);
var
  TmpParam : TMSParam;
begin
  TmpParam := FCommand.ParamByName(aParam.Name);

  if aParam.IsNull then
  begin
    TmpParam.DataType:= ParameterDataTypeToDataType(aParam.DataType);
    TmpParam.Clear;
    exit;
  end;

  case aParam.DataType of
    ptDate:
      TmpParam.AsDate := aParam.AsDate;
    ptTime:
      TmpParam.AsTime:= aParam.AsTime;
    ptDateTime:
      TmpParam.AsDateTime := aParam.AsDateTime;
    ptString:
      TmpParam.AsString := aParam.AsString;
    ptWideString:
      TmpParam.AsWideString:= aParam.AsWideString;
    ptInteger:
      TmpParam.AsInteger:= aParam.AsInteger;
    ptFloat:
      TmpParam.AsFloat:= aParam.AsFloat;
    else
      raise TmDataConnectionException.Create('Unknown parameter type');
  end;
end;

function TSdacDatabaseCommandImpl.GetParam(aIndex: integer): TParam;
begin
  Result := FCommand.Params[aIndex];
end;

(*
function TSdacDatabaseCommandImpl.ParamByName(const Value: string): TParam;
begin
  Result := FCommand.ParamByName(Value);
end;*)

function TSdacDatabaseCommandImpl.Prepared: boolean;
begin
  Result := FCommand.Prepared;
end;

{ TSdacDatabaseQueryImpl }

procedure TSdacDatabaseQueryImpl.SetDatabaseConnectionImpl(value: TmDatabaseConnectionImpl);
begin
  FConnectionImpl := value as TSdacDatabaseConnectionImpl;
  FQuery.Connection := FConnectionImpl.FConnection;
end;

function TSdacDatabaseQueryImpl.GetDatabaseConnectionImpl: TmDatabaseConnectionImpl;
begin
  Result := FConnectionImpl;
end;

constructor TSdacDatabaseQueryImpl.Create;
begin
  FQuery :=  TMSQuery.Create(nil);
end;

destructor TSdacDatabaseQueryImpl.Destroy;
begin
  FQuery.Free;
  inherited Destroy;
end;

procedure TSdacDatabaseQueryImpl.SetSQL(aValue: TStringList);
begin
  FQuery.SQL.Clear;
  FQuery.SQL.AddStrings(aValue);
end;

function TSdacDatabaseQueryImpl.SameSQL(aValue: TStringList): boolean;
begin
  Result := FQuery.SQL.Count <> aValue.Count;
  if (not Result) then
    Result := (CompareStr(FQuery.SQL.Text, aValue.Text) = 0);
end;

procedure TSdacDatabaseQueryImpl.SetUnidirectional(const aValue: boolean);
begin
  FQuery.UniDirectional:= aValue;
end;

procedure TSdacDatabaseQueryImpl.Open;
begin
  FQuery.Open;
end;

procedure TSdacDatabaseQueryImpl.Close;
begin
  FQuery.Close;
end;

procedure TSdacDatabaseQueryImpl.Prepare;
begin
  FQuery.Prepare;
end;

procedure TSdacDatabaseQueryImpl.Unprepare;
begin
  FQuery.UnPrepare;
end;

procedure TSdacDatabaseQueryImpl.First;
begin
  FQuery.First;
end;

procedure TSdacDatabaseQueryImpl.Next;
begin
  FQuery.Next;
end;

function TSdacDatabaseQueryImpl.Eof: boolean;
begin
  Result := FQuery.EOF;
end;

function TSdacDatabaseQueryImpl.AsDataset: TDataset;
begin
  Result := FQuery;
end;

(*procedure TSdacDatabaseQueryImpl.SetParamValue(aParam: TmQueryParameter);
begin
  Result := FQuery.ParamByName(Value);
end;*)

function TSdacDatabaseQueryImpl.ParamCount: integer;
begin
  Result := FQuery.ParamCount;
end;

procedure TSdacDatabaseQueryImpl.SetParamValue(aParam: TmQueryParameter);
var
  TmpParam : TMSParam;
begin
  TmpParam := FQuery.ParamByName(aParam.Name);

  if aParam.IsNull then
  begin
    TmpParam.DataType:= ParameterDataTypeToDataType(aParam.DataType);
    TmpParam.Clear;
    exit;
  end;

  case aParam.DataType of
    ptDate:
      TmpParam.AsDate:= aParam.AsDate;
    ptTime:
      TmpParam.AsTime:= aParam.AsTime;
    ptDateTime:
      TmpParam.AsDateTime := aParam.AsDateTime;
    ptString:
      TmpParam.AsString := aParam.AsString;
    ptWideString:
      TmpParam.AsWideString:= aParam.AsWideString;
    ptInteger:
      TmpParam.AsInteger:= aParam.AsInteger;
    ptFloat:
      TmpParam.AsFloat:= aParam.AsFloat;
    else
      raise TmDataConnectionException.Create('Unknown parameter type');
  end;
end;

function TSdacDatabaseQueryImpl.GetParam(aIndex: integer): TParam;
begin
  Result := FQuery.Params[aIndex];
end;

function TSdacDatabaseQueryImpl.Prepared: boolean;
begin
  Result := FQuery.Prepared;
end;


{ TSdacDatabaseConnection }

constructor TSdacDatabaseConnectionImpl.Create;
begin
  FConnection := TMSConnection.Create(nil);
  FConnection.Options.DefaultLockTimeout := 60000;
  // FConnection.Options.Provider:= prDirect;
end;

destructor TSdacDatabaseConnectionImpl.Destroy;
begin
  FConnection.Free;
  inherited Destroy;
end;

procedure TSdacDatabaseConnectionImpl.Connect;
begin
  if (not FConnection.Connected) then
  begin
    FConnection.Server:= FConnectionInfo.Server;
    FConnection.Database:= FConnectionInfo.DatabaseName;
    FConnection.Options.Provider:= MSClasses.prAuto;
    if (FConnectionInfo.WindowsIntegratedSecurity) then
    begin
      FConnection.Options.PersistSecurityInfo:= true;
      FConnection.Authentication:= auWindows;
    end
    else
    begin
      FConnection.Username := FConnectionInfo.UserName;
      FConnection.Password := FConnectionInfo.Password;
    end;

    {$IFDEF WINDOWS}
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    {$ENDIF}
    FConnection.Connect;
  end;
end;

procedure TSdacDatabaseConnectionImpl.Close;
begin
  if FConnection.Connected then
    FConnection.Close();
end;

function TSdacDatabaseConnectionImpl.GetName: String;
begin
  Result := TSdacDatabaseConnectionImpl.GetImplementationName;
end;

procedure TSdacDatabaseConnectionImpl.StartTransaction;
begin
  FConnection.StartTransaction;
end;

procedure TSdacDatabaseConnectionImpl.Commit;
begin
  FConnection.Commit;
end;

procedure TSdacDatabaseConnectionImpl.Rollback;
begin
  FConnection.Rollback;
end;

class function TSdacDatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'sdac';
end;

initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TSdacDatabaseConnectionImpl.GetImplementationName, dvSQLServer, '', TSdacDatabaseConnectionImpl, TSdacDatabaseQueryImpl, TSdacDatabaseCommandImpl);

end.
