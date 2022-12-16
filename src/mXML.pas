// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mXML;

interface

{$I mDefines.inc}

uses
  Classes, SysUtils, {$IFDEF GRAPHICS_AVAILABLE}Graphics,{$ENDIF}
  mNullables;

type
  TmXMLString = string;

  EmXmlError = class(Exception);

  TImpl_mXmlElement = class;
  TImpl_mXmlDocument = class;
  TImpl_mXmlElementCursor = class;

  TmXmlElement = class;

  { TmXmlDocument }

  TmXmlDocument = class
  private
    FImpl : TImpl_mXmlDocument;
  public
    constructor Create;
    destructor Destroy; override;

    function RootElement: TmXmlElement;
    function CreateRootElement(Name: string): TmXmlElement;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure SaveToFileEncrypted (FileName : string; Password : String);
    procedure LoadFromFileEncrypted (FileName : string; Password : String);
  end;

  { TmXmlElement }

  TmXmlElement = class
  private
    FImpl : TImpl_mXmlElement;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;

    function AddElement(Name: TmXMLString): TmXmlElement;
    function HasAttribute(const Name: TmXMLString): boolean;
    procedure DeleteAttribute(const aName : TmXMLString);
    procedure SetAttribute(aName, aValue: TmXmlString); overload;
    procedure SetAttribute(aName: TmXmlString; const aValue: TNullableString); overload;
    function GetAttribute(const aName: TmXmlString): TmXmlString; overload;
    function GetAttribute(const aName: TmXmlString; const aDefaultValue: TmXmlString): TmXmlString; overload;
    procedure GetAttribute(const aName: TmXmlString; const aValue: TNullableString);
    procedure SetDateTimeAttribute(const aName : TmXmlString; const aValue: TDateTime); overload;
    procedure SetDateTimeAttribute(const aName: TmXmlString; const aValue: TNullableDateTime); overload;
    function GetDateTimeAttribute(const aName: TmXmlString): TDateTime; overload;
    function GetDateTimeAttribute(const aName: TmXmlString; const aDefaultValue : TDateTime): TDateTime; overload;
    procedure GetDateTimeAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
    procedure SetDateAttribute(const aName : TmXmlString; const aValue: TDateTime); overload;
    procedure SetDateAttribute(const aName: TmXmlString; const aValue: TNullableDateTime); overload;
    function GetDateAttribute(const aName: TmXmlString): TDateTime; overload;
    function GetDateAttribute(const aName: TmXmlString; const aDefaultValue : TDateTime): TDateTime; overload;
    procedure GetDateAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
    procedure SetFloatAttribute(const aName : TmXmlString; const aValue: Double); overload;
    procedure SetFloatAttribute(const aName: TmXmlString; const aValue: TNullableDouble); overload;
    function GetFloatAttribute(const aName: TmXmlString): Double; overload;
    function GetFloatAttribute(const aName: TmXmlString; const aDefaultValue : Double): Double; overload;
    procedure GetFloatAttribute(const aName: TmXmlString; aValue: TNullableDouble);
    procedure SetIntegerAttribute(const aName: TmXmlString; const aValue: integer); overload;
    procedure SetIntegerAttribute(const aName: TmXmlString; const aValue: TNullableInteger); overload;
    function GetIntegerAttribute(const aName: TmXmlString): integer; overload;
    function GetIntegerAttribute(const aName: TmXmlString; const aDefaultValue : integer): integer; overload;
    procedure GetIntegerAttribute(const aName: TmXmlString; aValue : TNullableInteger);
    procedure SetBooleanAttribute(Name : TmXmlString; Value : Boolean);
    function GetBooleanAttribute(Name: TmXMLString): boolean; overload;
    function GetBooleanAttribute(Name: TmXMLString; DefaultValue : boolean): boolean; overload;
    {$IFDEF GRAPHICS_AVAILABLE}
    procedure SetColorAttribute(const aName: TmXmlString; const aValue: TColor); overload;
    procedure SetColorAttribute(const aName: TmXmlString; const aValue: TNullableColor); overload;
    function GetColorAttribute(const aName: TmXmlString): TColor; overload;
    function GetColorAttribute(const aName: TmXmlString; const aDefaultValue : TColor): TColor; overload;
    procedure GetColorAttribute(const aName: TmXmlString; aValue : TNullableColor);
    {$ENDIF}

    procedure SetValue(const aValue: TmXMLString); overload;
    procedure SetValue(const aValue: TNullableString); overload;
    procedure SetDateValue(const aValue: TDateTime);
    procedure SetDateTimeValue(const aValue: TDateTime);
    procedure SetIntegerValue(const aValue: Integer);
    procedure SetFloatValue(const aValue: Double);
    function GetValue: TmXMLString;
    function GetDateValue: TDateTime;
    function GetDateTimeValue: TDateTime;
    function GetIntegerValue: Integer;
    function GetFloatValue: Double;

    function HasValue: boolean;
  end;

  TmXmlElementCursor = class
  private
    FImpl : TImpl_mXmlElementCursor;
    function GetElement(I: integer): TmXmlElement;
  public
    constructor Create(Element: TmXmlElement; Name: TmXmlString); reintroduce; overload;
    constructor Create(Element: TmXmlElement); reintroduce; overload;
    destructor Destroy; override;
    function Count : integer;
  public
    property Elements[I: integer]: TmXmlElement read GetElement; default;
  end;

  { TImpl_mXmlElement }

  TImpl_mXmlElement = class
  public
    constructor Create; virtual; abstract;
  public
    function _AddElement(const aName: TmXMLString): TmXmlElement; virtual; abstract;
    function _HasAttribute(const aName: TmXMLString): boolean; virtual; abstract;
    procedure _DeleteAttribute(const aName: TmXMLString); virtual; abstract;
    procedure _SetAttribute(const aName, aValue: TmXmlString); virtual; abstract;
    function _GetAttribute(const aName: TmXmlString): TmXmlString; overload; virtual; abstract;
    function _GetAttribute(const aName: TmXmlString; const aDefault: TmXmlString): TmXmlString; overload; virtual; abstract;
    procedure _SetDateTimeAttribute(const aName : TmXmlString; const aValue : TDateTime); virtual; abstract;
    function _GetDateTimeAttribute(const aName: TmXmlString): TDateTime; overload; virtual; abstract;
    function _GetDateTimeAttribute(const aName: TmXmlString; const aDefault : TDateTime): TDateTime; overload; virtual; abstract;
    procedure _SetDateAttribute(const aName : TmXmlString; const aValue : TDateTime); virtual; abstract;
    function _GetDateAttribute(const aName: TmXmlString): TDateTime; overload; virtual; abstract;
    function _GetDateAttribute(const aName: TmXmlString; const aDefault : TDateTime): TDateTime; overload; virtual; abstract;
    procedure _SetFloatAttribute(const aName : TmXmlString; const aValue : double); virtual; abstract;
    function _GetFloatAttribute(const aName: TmXmlString): double; overload; virtual; abstract;
    function _GetFloatAttribute(const aName: TmXmlString; const aDefault : double): double; overload; virtual; abstract;
    procedure _SetIntegerAttribute(const aName : TmXmlString; const aValue: integer); virtual; abstract;
    function _GetIntegerAttribute(const aName: TmXmlString): integer; overload; virtual; abstract;
    function _GetIntegerAttribute(const aName: TmXmlString; const aDefault : integer): integer; overload; virtual; abstract;
    function _HasValue: boolean; virtual; abstract;
    procedure _SetValue(const aValue: TmXMLString); overload; virtual; abstract;
    function _GetValue: TmXMLString; virtual; abstract;
    procedure _SetDateTimeValue(const aValue : TDateTime); virtual; abstract;
    function _GetDateTimeValue: TDateTime; virtual; abstract;
    procedure _SetDateValue(const aValue : TDateTime); virtual; abstract;
    function _GetDateValue: TDateTime; virtual; abstract;
    procedure _SetFloatValue(const aValue : double); virtual; abstract;
    function _GetFloatValue: double; virtual; abstract;
    procedure _SetIntegerValue(const aValue : integer); virtual; abstract;
    function _GetIntegerValue: integer; virtual; abstract;

    procedure _SetOwner(aOwner : TObject); virtual; abstract;
  end;

  TImpl_mXmlDocument = class
  public
    constructor Create; virtual; abstract;
    function _RootElement: TmXmlElement; virtual; abstract;
    function _CreateRootElement(const aName: string): TmXmlElement; virtual; abstract;
    procedure _Clear; virtual; abstract;
    procedure _SaveToStream(aStream: TStream); virtual; abstract;
    procedure _LoadFromStream(aStream: TStream); virtual; abstract;
    procedure _SaveToFile(const aFileName: string); virtual; abstract;
    procedure _LoadFromFile(const aFileName: string); virtual; abstract;
    procedure _SaveToFileEncrypted (const aFileName : string; const aPassword : String); virtual; abstract;
    procedure _LoadFromFileEncrypted (const aFileName : string; const aPassword : String); virtual; abstract;
  end;

  TImpl_mXmlElementCursor = class
  public
    constructor Create; virtual; abstract;

    procedure _SetParent(aParent : TImpl_mXmlElement; aFilter : TmXMLString); virtual; abstract;
    function _GetElement(I : integer) : TmXmlElement; virtual; abstract;
    function _Count : integer; virtual; abstract;
  end;

  TImpl_Factory = class abstract
  public
    function GetTImpl_mXmlElement : TImpl_mXmlElement; virtual; abstract;
    function GetTImpl_mXmlDocument : TImpl_mXmlDocument; virtual; abstract;
    function GetTImpl_mXmlElementCursor : TImpl_mXmlElementCursor; virtual; abstract;
  end;

  TImpl_FactoryClass = class of TImpl_Factory;

  procedure RegisterXMLImplFactory(aFactoryClass : TImpl_FactoryClass);

implementation

uses
  {$IFDEF FPC}mXML_fpxml,{$ELSE}mXML_oxml, {$ENDIF} mUtility;

var
  InternalFactory : TImpl_Factory;

procedure RegisterXMLImplFactory(aFactoryClass: TImpl_FactoryClass);
begin
  InternalFactory := aFactoryClass.Create;
end;



{ TmXmlElement }

function TmXmlElement.AddElement(Name: TmXMLString): TmXmlElement;
begin
  Result := FImpl._AddElement(Name);
end;

constructor TmXmlElement.Create(Owner: TObject);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElement;
  FImpl._SetOwner(Owner);
end;

destructor TmXmlElement.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TmXmlElement.GetAttribute(const aName: TmXmlString; const aDefaultValue: TmXmlString): TmXmlString;
begin
  Result := FImpl._GetAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetAttribute(const aName: TmXmlString; const aValue: TNullableString);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetAttribute(aName)
  else
    aValue.IsNull:= true;
end;

function TmXmlElement.GetDateTimeAttribute(const aName: TmXmlString): TDateTime;
begin
  Result := FImpl._GetDateTimeAttribute(aName);
end;

function TmXmlElement.GetDateTimeAttribute(const aName: TmXmlString; const aDefaultValue: TDateTime): TDateTime;
begin
  Result := FImpl._GetDateTimeAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetDateTimeAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetDateTimeAttribute(aName)
  else
    aValue.IsNull:= true;
end;

procedure TmXmlElement.SetDateAttribute(const aName: TmXmlString; const aValue: TDateTime);
begin
  FImpl._SetDateAttribute(aName, aValue);
end;

procedure TmXmlElement.SetDateAttribute(const aName: TmXmlString; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    Self.SetDateAttribute(aName, aValue.Value)
  else
    Self.DeleteAttribute(aName);
end;

function TmXmlElement.GetDateAttribute(const aName: TmXmlString): TDateTime;
begin
  Result := FImpl._GetDateAttribute(aName);
end;

function TmXmlElement.GetDateAttribute(const aName: TmXmlString; const aDefaultValue: TDateTime): TDateTime;
begin
  Result := FImpl._GetDateAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetDateAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetDateAttribute(aName)
  else
    aValue.IsNull:= true;
end;

procedure TmXmlElement.SetFloatAttribute(const aName: TmXmlString; const aValue: Double);
begin
  FImpl._SetFloatAttribute(aName, aValue);
end;

procedure TmXmlElement.SetFloatAttribute(const aName: TmXmlString; const aValue: TNullableDouble);
begin
  if aValue.NotNull then
    Self.SetFloatAttribute(aName, aValue.AsFloat)
  else
    Self.DeleteAttribute(aName);
end;

function TmXmlElement.GetFloatAttribute(const aName: TmXmlString): Double;
begin
  Result := FImpl._GetFloatAttribute(aName);
end;

function TmXmlElement.GetFloatAttribute(const aName: TmXmlString; const aDefaultValue: Double): Double;
begin
  Result := FImpl._GetFloatAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetFloatAttribute(const aName: TmXmlString; aValue: TNullableDouble);
begin
  if Self.HasAttribute(aName) then
    aValue.Value := Self.GetFloatAttribute(aName)
  else
    aValue.IsNull:= true;
end;

function TmXmlElement.GetIntegerAttribute(const aName: TmXmlString): integer;
begin
  Result := FImpl._GetIntegerAttribute(aName);
end;

function TmXmlElement.GetIntegerAttribute(const aName: TmXmlString; const aDefaultValue: integer): integer;
begin
  Result := FImpl._GetIntegerAttribute(aName, aDefaultValue)
end;

procedure TmXmlElement.GetIntegerAttribute(const aName: TmXmlString; aValue: TNullableInteger);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetIntegerAttribute(aName)
  else
    aValue.IsNull:= true;
end;

procedure TmXmlElement.SetBooleanAttribute(Name: TmXmlString; Value: Boolean);
begin
  Self.SetAttribute(Name, BoolToStr(Value, true));
end;

function TmXmlElement.GetBooleanAttribute(Name: TmXMLString): boolean;
begin
  Result := StrToBool(Self.GetAttribute(Name));
end;

function TmXmlElement.GetBooleanAttribute(Name: TmXMLString; DefaultValue: boolean): boolean;
begin
  Result := StrToBool(Self.GetAttribute(Name, BoolToStr(DefaultValue, true)));
end;

{$IFDEF GRAPHICS_AVAILABLE}
procedure TmXmlElement.SetColorAttribute(const aName: TmXmlString; const aValue: TColor);
begin
  Self.SetAttribute(aName, ColorToString(aValue));
end;
{$ENDIF}

{$IFDEF GRAPHICS_AVAILABLE}
procedure TmXmlElement.SetColorAttribute(const aName: TmXmlString; const aValue: TNullableColor);
begin
  if aValue.NotNull  then
    Self.SetColorAttribute(aName, aValue.Value)
  else
    Self.DeleteAttribute(aName);
end;
{$ENDIF}

{$IFDEF GRAPHICS_AVAILABLE}
function TmXmlElement.GetColorAttribute(const aName: TmXmlString): TColor;
begin
  Result := GetColorAttribute(aName, clNone);
end;
{$ENDIF}

{$IFDEF GRAPHICS_AVAILABLE}
function TmXmlElement.GetColorAttribute(const aName: TmXmlString; const aDefaultValue: TColor): TColor;
var
  str: String;
  tmpColor: TColor;
begin
  str:= GetAttribute(aName);
  if TryToUndestandColorString(str, tmpColor) then
    Result := tmpColor
  else
    Result := aDefaultValue;
end;
{$ENDIF}

{$IFDEF GRAPHICS_AVAILABLE}
procedure TmXmlElement.GetColorAttribute(const aName: TmXmlString; aValue: TNullableColor);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetColorAttribute(aName)
  else
    aValue.IsNull:= true;
end;
{$ENDIF}

procedure TmXmlElement.SetValue(const aValue: TmXMLString);
begin
  FImpl._SetValue(aValue);
end;

procedure TmXmlElement.SetValue(const aValue: TNullableString);
begin
  if aValue.NotNull then
    FImpl._SetValue(aValue.AsString);
end;

procedure TmXmlElement.SetDateValue(const aValue: TDateTime);
begin
  FImpl._SetDateValue(aValue);
end;

procedure TmXmlElement.SetDateTimeValue(const aValue: TDateTime);
begin
  FImpl._SetDateTimeValue(aValue);
end;

procedure TmXmlElement.SetIntegerValue(const aValue: Integer);
begin
  FImpl._SetIntegerValue(aValue);
end;

procedure TmXmlElement.SetFloatValue(const aValue: Double);
begin
  FImpl._SetFloatValue(aValue);
end;

function TmXmlElement.GetValue: TmXMLString;
begin
  Result := FImpl._GetValue;
end;

function TmXmlElement.GetDateValue: TDateTime;
begin
  Result := FImpl._GetDateValue;
end;

function TmXmlElement.GetDateTimeValue: TDateTime;
begin
  Result := FImpl._GetDateTimeValue;
end;

function TmXmlElement.GetIntegerValue: Integer;
begin
  Result := FImpl._GetIntegerValue;
end;

function TmXmlElement.GetFloatValue: Double;
begin
  Result := FImpl._GetFloatValue;
end;

function TmXmlElement.HasValue: boolean;
begin
  Result := FImpl._HasValue;
end;

function TmXmlElement.GetAttribute(const aName: TmXmlString): TmXmlString;
begin
  Result := FImpl._GetAttribute(aName);
end;

function TmXmlElement.HasAttribute(const Name: TmXMLString): boolean;
begin
  Result := FImpl._HasAttribute(Name);
end;

procedure TmXmlElement.DeleteAttribute(const aName: TmXMLString);
begin
  FImpl._DeleteAttribute(aName);
end;


procedure TmXmlElement.SetAttribute(aName, aValue: TmXmlString);
begin
  FImpl._SetAttribute(aName, aValue);
end;

procedure TmXmlElement.SetAttribute(aName: TmXmlString; const aValue: TNullableString);
begin
  if aValue.NotNull then
    Self.SetAttribute(aName, aValue.Value)
  else
    Self.DeleteAttribute(aName);
end;

procedure TmXmlElement.SetDateTimeAttribute(const aName: TmXmlString; const aValue: TDateTime);
begin
  FImpl._SetDateTimeAttribute(aName, aValue);
end;

procedure TmXmlElement.SetDateTimeAttribute(const aName: TmXmlString; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    Self.SetDateTimeAttribute(aName, aValue.Value)
  else
    Self.DeleteAttribute(aName);
end;

procedure TmXmlElement.SetIntegerAttribute(const aName: TmXmlString; const aValue: integer);
begin
  FImpl._SetIntegerAttribute(aName, aValue);
end;

procedure TmXmlElement.SetIntegerAttribute(const aName: TmXmlString; const aValue: TNullableInteger);
begin
  if aValue.NotNull  then
    Self.SetIntegerAttribute(aName, aValue.AsInteger)
  else
    Self.DeleteAttribute(aName);
end;

{ TmXmlDocument }

procedure TmXmlDocument.Clear;
begin
  FImpl._Clear;
end;

constructor TmXmlDocument.Create;
begin
  FImpl := InternalFactory.GetTImpl_mXmlDocument;
end;

function TmXmlDocument.CreateRootElement(Name: string): TmXmlElement;
begin
  Result := FImpl._CreateRootElement(Name);
end;

destructor TmXmlDocument.Destroy;
begin
  FImpl.Free;
  inherited;
end;

procedure TmXmlDocument.LoadFromFile(FileName: string);
begin
  FImpl._LoadFromFile(FileName);
end;

procedure TmXmlDocument.SaveToFileEncrypted(FileName: string; Password: String);
begin
  FImpl._SaveToFileEncrypted(FileName, Password);
end;

procedure TmXmlDocument.LoadFromFileEncrypted(FileName: string; Password: String);
begin
  FImpl._LoadFromFileEncrypted(FileName, Password);
end;

procedure TmXmlDocument.LoadFromStream(Stream: TStream);
begin
  FImpl._LoadFromStream(Stream);
end;

function TmXmlDocument.RootElement: TmXmlElement;
begin
  Result := FImpl._RootElement;
end;

procedure TmXmlDocument.SaveToFile(FileName: string);
begin
  FImpl._SaveToFile(FileName);
end;

procedure TmXmlDocument.SaveToStream(Stream: TStream);
begin
  FImpl._SaveToStream(Stream);
end;

{ TmXmlElementCursor }

constructor TmXmlElementCursor.Create(Element: TmXmlElement; Name: TmXmlString);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElementCursor;
  FImpl._SetParent(Element.FImpl, Name);
end;

function TmXmlElementCursor.Count: integer;
begin
  Result := FImpl._Count;
end;

constructor TmXmlElementCursor.Create(Element: TmXmlElement);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElementCursor;
  FImpl._SetParent(Element.FImpl, '');
end;

destructor TmXmlElementCursor.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TmXmlElementCursor.GetElement(I: integer): TmXmlElement;
begin
  Result := FImpl._GetElement(I);
end;

initialization
  {$IFDEF FPC}
  RegisterXMLImplFactory(TImpl_Factory_fpxml);
  {$ELSE}
  RegisterXMLImplFactory(TImpl_Factory_oxml);
  {$ENDIF}

finalization
  FreeAndNil(InternalFactory);
end.
