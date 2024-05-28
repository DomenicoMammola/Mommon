// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImpl;

{$MODE DELPHI}

interface

uses
  Classes, DB,
  mDatabaseConnectionClasses;

type

  { TmDatabaseConnectionImpl }

  TmDatabaseConnectionImpl = class abstract
  private
    protected
      FConnectionInfo : TmDatabaseConnectionInfo;
      procedure SetConnectionInfo(AValue: TmDatabaseConnectionInfo); virtual;
    public
      constructor Create; virtual; abstract;
      procedure Connect; virtual; abstract;
      procedure Close; virtual; abstract;
      function GetName : String; virtual; abstract;
      procedure StartTransaction; virtual; abstract;
      procedure EndTransaction; virtual; abstract;
      procedure Commit; virtual; abstract;
      procedure Rollback; virtual; abstract;
      function Connected : boolean; virtual; abstract;

      property ConnectionInfo : TmDatabaseConnectionInfo read FConnectionInfo write SetConnectionInfo;
  end;

  TmDatabaseConnectionImplClass = class of TmDatabaseConnectionImpl;

  TmAbstractDatabaseCommandImpl = class abstract
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); virtual; abstract;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; virtual; abstract;
  public
    constructor Create; virtual; abstract;

    procedure SetSQL (aValue : TStringList); virtual; abstract;
    function SameSQL (aValue : TStringList): boolean; virtual; abstract;
    procedure Prepare; virtual; abstract;
    //function ParamByName(const Value: string): TParam; virtual; abstract;
    function ParamCount : integer; virtual; abstract;
    function GetParam (aIndex : integer) : TParam; virtual; abstract;
    procedure SetParamValue(aParam : TmQueryParameter); virtual; abstract;
    function Prepared : boolean; virtual; abstract;
    procedure Unprepare; virtual; abstract;
    procedure SetParamCheck(const aValue : boolean); virtual; abstract;
    function GetParamCheck : boolean; virtual; abstract;

    property DatabaseConnectionImpl : TmDatabaseConnectionImpl read GetDatabaseConnectionImpl write SetDatabaseConnectionImpl;
  end;

  { TmDatabaseQueryImpl }

  TmDatabaseQueryImpl = class (TmAbstractDatabaseCommandImpl)
  public
    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Next; virtual; abstract;
    function Eof : boolean; virtual; abstract;
    function AsDataset : TDataset; virtual; abstract;
    function GetUnidirectional : boolean; virtual; abstract;
    procedure SetUnidirectional(const aValue : boolean); virtual; abstract;
  end;

  TmDatabaseQueryImplClass = class of TmDatabaseQueryImpl;

  TmDatabaseCommandImpl = class (TmAbstractDatabaseCommandImpl)
  public
    function Execute : integer; virtual; abstract;
  end;

  TmDatabaseCommandImplClass = class of TmDatabaseCommandImpl;

implementation

{ TmDatabaseConnectionImpl }

procedure TmDatabaseConnectionImpl.SetConnectionInfo(AValue: TmDatabaseConnectionInfo);
begin
  if FConnectionInfo=AValue then Exit;
  FConnectionInfo:=AValue;
end;

end.
