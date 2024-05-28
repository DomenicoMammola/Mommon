// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImplOnSqldb;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,

  sqldb,

  mDatabaseConnectionClasses,
  mDatabaseConnectionImpl;

type


  { TAbstractSqldbDatabaseConnectionImpl }

  TAbstractSqldbDatabaseConnectionImpl = class(TmDatabaseConnectionImpl)
  protected
    FConnection : TSQLConnection;
    FTransaction : TSQLTransaction;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function Connected: boolean; override;
  end;

  { TAbstractSqldbDatabaseQueryImpl }

  TAbstractSqldbDatabaseQueryImpl = class (TmDatabaseQueryImpl)
  private
    FConnectionImpl : TAbstractSqldbDatabaseConnectionImpl;
    FQuery: TSQLQuery;
    FPrepared : boolean;
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); override;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetSQL (aValue : TStringList); override;
    function SameSQL (aValue : TStringList): boolean; override;

    procedure Open; override;
    procedure Close; override;
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure First; override;
    procedure Next; override;
    function Eof : boolean; override;
    function AsDataset : TDataset; override;
    function ParamCount : integer; override;
    procedure SetParamValue(aParam : TmQueryParameter); override;
    function GetParam (aIndex : integer) : TParam; override;
    function Prepared : boolean; override;
    function GetUnidirectional : boolean; override;
    procedure SetUnidirectional(const aValue : boolean); override;
    procedure SetParamCheck(const aValue : boolean); override;
    function GetParamCheck : boolean; override;
  end;

  { TAbstractSqldbDatabaseCommandImpl }

  TAbstractSqldbDatabaseCommandImpl = class (TmDatabaseCommandImpl)
  private
    FConnectionImpl : TAbstractSqldbDatabaseConnectionImpl;
    FCommand : TSQLQuery;
    FPrepared : boolean;
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
    function ParamCount : integer; override;
    procedure SetParamValue(aParam : TmQueryParameter); override;
    function GetParam (aIndex : integer) : TParam; override;
    procedure SetParamCheck(const aValue : boolean); override;
    function GetParamCheck : boolean; override;

    function Prepared : boolean; override;
  end;



implementation

uses
  SysUtils, syncobjs;

var
  _ConnectionCriticalSection : TCriticalSection;

{ TAbstractSqldbDatabaseCommandImpl }

procedure TAbstractSqldbDatabaseCommandImpl.SetDatabaseConnectionImpl(value: TmDatabaseConnectionImpl);
begin
  FConnectionImpl := value as TAbstractSqldbDatabaseConnectionImpl;
  FCommand.DataBase := FConnectionImpl.FConnection;
end;

function TAbstractSqldbDatabaseCommandImpl.GetDatabaseConnectionImpl: TmDatabaseConnectionImpl;
begin
  Result := FConnectionImpl;
end;

constructor TAbstractSqldbDatabaseCommandImpl.Create;
begin
  FCommand := TSQLQuery.Create(nil);
  FCommand.ParamCheck:= DefaultParamCheck;
  FPrepared := false;
end;

destructor TAbstractSqldbDatabaseCommandImpl.Destroy;
begin
  FreeAndNil(FCommand);
  inherited Destroy;
end;

procedure TAbstractSqldbDatabaseCommandImpl.SetSQL(aValue: TStringList);
begin
  FCommand.SQL.Clear;
  FCommand.SQL.AddStrings(aValue);
  FPrepared := false;
end;

function TAbstractSqldbDatabaseCommandImpl.SameSQL(aValue: TStringList): boolean;
begin
  Result := FCommand.SQL.Count <> aValue.Count;
  if (not Result) then
    Result := (CompareStr(FCommand.SQL.Text, aValue.Text) = 0);
end;

function TAbstractSqldbDatabaseCommandImpl.Execute: integer;
begin
  FCommand.ExecSQL();
  Result := FCommand.RowsAffected;
  FPrepared := true;
end;

procedure TAbstractSqldbDatabaseCommandImpl.Prepare;
begin
  FCommand.Prepare;
  FPrepared := true;
end;

procedure TAbstractSqldbDatabaseCommandImpl.Unprepare;
begin
  FCommand.Unprepare;
  FPrepared := false;
end;

function TAbstractSqldbDatabaseCommandImpl.ParamCount: integer;
begin
  Result := FCommand.Params.Count;
end;

procedure TAbstractSqldbDatabaseCommandImpl.SetParamValue(aParam: TmQueryParameter);
var
  TmpParam : TParam;
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

function TAbstractSqldbDatabaseCommandImpl.GetParam(aIndex: integer): TParam;
begin
  Result := FCommand.Params[aIndex];
end;

procedure TAbstractSqldbDatabaseCommandImpl.SetParamCheck(const aValue: boolean);
begin
  FCommand.ParamCheck:= aValue;
end;

function TAbstractSqldbDatabaseCommandImpl.GetParamCheck: boolean;
begin
  Result := FCommand.ParamCheck;
end;

function TAbstractSqldbDatabaseCommandImpl.Prepared: boolean;
begin
  Result := FPrepared;
end;

{ TAbstractSqldbDatabaseQueryImpl }

procedure TAbstractSqldbDatabaseQueryImpl.SetDatabaseConnectionImpl(value: TmDatabaseConnectionImpl);
begin
  FConnectionImpl := value as TAbstractSqldbDatabaseConnectionImpl;
  FQuery.DataBase := FConnectionImpl.FConnection;
end;

function TAbstractSqldbDatabaseQueryImpl.GetDatabaseConnectionImpl: TmDatabaseConnectionImpl;
begin
  Result := FConnectionImpl;
end;

constructor TAbstractSqldbDatabaseQueryImpl.Create;
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.UniDirectional:= true;
  FQuery.ParseSQL := False;
  FQuery.ReadOnly := True;
  FQuery.ParamCheck:= DefaultParamCheck;
  FPrepared := false;
end;

destructor TAbstractSqldbDatabaseQueryImpl.Destroy;
begin
  FreeAndNil(FQuery);
  inherited Destroy;
end;

procedure TAbstractSqldbDatabaseQueryImpl.SetSQL(aValue: TStringList);
begin
  FQuery.SQL.Clear;
  FQuery.SQL.AddStrings(aValue);
end;

function TAbstractSqldbDatabaseQueryImpl.SameSQL(aValue: TStringList): boolean;
begin
  Result := FQuery.SQL.Count <> aValue.Count;
  if (not Result) then
    Result := (CompareStr(FQuery.SQL.Text, aValue.Text) = 0);
end;

procedure TAbstractSqldbDatabaseQueryImpl.Open;
begin
  if not FPrepared then
    FPrepared := true;
  FQuery.Open;
end;

procedure TAbstractSqldbDatabaseQueryImpl.Close;
begin
  FQuery.Close;
end;

procedure TAbstractSqldbDatabaseQueryImpl.Prepare;
begin
  FQuery.Prepare;
  FPrepared := true;
end;

procedure TAbstractSqldbDatabaseQueryImpl.Unprepare;
begin
  FQuery.UnPrepare;
  FPrepared := false;
end;

procedure TAbstractSqldbDatabaseQueryImpl.First;
begin
  FQuery.First;
end;

procedure TAbstractSqldbDatabaseQueryImpl.Next;
begin
  FQuery.Next;
end;

function TAbstractSqldbDatabaseQueryImpl.Eof: boolean;
begin
  Result := FQuery.EOF;
end;

function TAbstractSqldbDatabaseQueryImpl.AsDataset: TDataset;
begin
  Result := FQuery;
end;

function TAbstractSqldbDatabaseQueryImpl.ParamCount: integer;
begin
  Result := FQuery.Params.Count;
end;

procedure TAbstractSqldbDatabaseQueryImpl.SetParamValue(aParam: TmQueryParameter);
var
  TmpParam : TParam;
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

function TAbstractSqldbDatabaseQueryImpl.GetParam(aIndex: integer): TParam;
begin
  Result := FQuery.Params[aIndex];
end;

function TAbstractSqldbDatabaseQueryImpl.Prepared: boolean;
begin
  Result := FPrepared;
end;

function TAbstractSqldbDatabaseQueryImpl.GetUnidirectional: boolean;
begin
  Result := FQuery.UniDirectional;
end;

procedure TAbstractSqldbDatabaseQueryImpl.SetUnidirectional(const aValue: boolean);
begin
  FQuery.UniDirectional:= aValue;
end;

procedure TAbstractSqldbDatabaseQueryImpl.SetParamCheck(const aValue: boolean);
begin
  FQuery.ParamCheck:= aValue;
end;

function TAbstractSqldbDatabaseQueryImpl.GetParamCheck: boolean;
begin
  Result := FQuery.ParamCheck;
end;

{ TAbstractSqldbDatabaseConnectionImpl }

constructor TAbstractSqldbDatabaseConnectionImpl.Create();
begin
  FConnection := nil;
  FTransaction := TSQLTransaction.Create(nil);
end;

destructor TAbstractSqldbDatabaseConnectionImpl.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

procedure TAbstractSqldbDatabaseConnectionImpl.Connect;
begin
  _ConnectionCriticalSection.Acquire;
  try
    if (not FConnection.Connected) then
    begin
      FConnection.HostName:= FConnectionInfo.Server;
      FConnection.DatabaseName:= FConnectionInfo.DatabaseName;
      if (not FConnectionInfo.WindowsIntegratedSecurity) then
      begin
        FConnection.Username := FConnectionInfo.UserName;
        FConnection.Password := FConnectionInfo.Password;
      end;
      FConnection.Open;
    end;
  finally
    _ConnectionCriticalSection.Leave;
  end;
end;

procedure TAbstractSqldbDatabaseConnectionImpl.Close;
begin
  if FConnection.Connected then
    FConnection.Close(false);
end;

procedure TAbstractSqldbDatabaseConnectionImpl.StartTransaction;
begin
  FTransaction.StartTransaction;
end;

procedure TAbstractSqldbDatabaseConnectionImpl.EndTransaction;
begin
  FTransaction.EndTransaction;
end;

procedure TAbstractSqldbDatabaseConnectionImpl.Commit;
begin
  FTransaction.Commit;
end;

procedure TAbstractSqldbDatabaseConnectionImpl.Rollback;
begin
  FTransaction.Rollback;
end;

function TAbstractSqldbDatabaseConnectionImpl.Connected: boolean;
begin
  Result := FConnection.Connected;
end;


initialization
  _ConnectionCriticalSection := TCriticalSection.Create;

finalization

  FreeAndNil(_ConnectionCriticalSection);

end.
