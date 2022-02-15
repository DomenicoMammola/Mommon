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
// This is the implementation of the mXML unit on the OXml library
// which can be found at:
// http://www.kluug.net/oxml.php

unit mXML_oxml;

interface

uses
  Classes, Contnrs, sysutils,
  mXML, OXmlPDOM;

resourcestring
 mXML_oxml_AttributeNotFound = 'XML attribute %s not found.';
 mXML_oxml_WrongDateTimeAttribute = 'XML attribute %s has value %s which is not in ISO date-time format.';
 mXML_oxml_WrongDateAttribute = 'XML attribute %s has value $s which is not in ISO date format.';
 mXML_oxml_WrongFloatAttribute = 'XML attribute %s has value %s that is not a valid float number.';
 mXML_oxml_WrongIntegerAttribute = 'XML attribute %s has value %s that is not a valid integer number.';

 mXML_oxml_ValueNotFound = 'XML element %s has no value.';
 mXML_oxml_WrongDateTimeValue = 'XML element %s has value %s which is not in ISO date-time format.';
 mXML_oxml_WrongDateValue = 'XML element %s has value $s which is not in ISO date format.';
 mXML_oxml_WrongFloatValue = 'XML element %s has value %s that is not a valid float number.';
 mXML_oxml_WrongIntegerValue = 'XML element %s has value %s that is not a valid integer number.';

type
  Tmxml_oxml_PointerShell = class
  public
    MyPointer : pointer;
    constructor Create(aPointer : pointer);
  end;

  { TImpl_oxml_mXmlElement }

  TImpl_oxml_mXmlElement = class (TImpl_mXmlElement)
  private
    FNode : PXMLNode;
    FGarbage : TObjectList;
    FFormatSettings : TFormatSettings;
    procedure RaiseMissingAttributeException(const Name: TmXMLString);
    procedure RaiseWrongDateTimeAttributeException(const Name, Value: TmXMLString);
    procedure RaiseWrongDateAttributeException(const Name, Value: TmXMLString);
    procedure RaiseWrongFloatAttributeException(const Name, Value: TmXMLString);
    procedure RaiseWrongIntegerAttributeException(const Name, Value: TmXMLString);
    procedure RaiseMissingValueException(const aNodeName : TmXMLString);
    procedure RaiseWrongDateTimeValueException(const aNodeName, Value: TmXMLString);
    procedure RaiseWrongDateValueException(const aNodeName, Value: TmXMLString);
    procedure RaiseWrongFloatValueException(const aNodeName, Value: TmXMLString);
    procedure RaiseWrongIntegerValueException(const aNodeName, Value: TmXMLString);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    function _AddElement(const aName: TmXMLString): TmXmlElement; override;
    function _HasAttribute(const aName: TmXMLString): boolean; override;
    procedure _DeleteAttribute(const aName: TmXMLString); override;
    procedure _SetAttribute(const aName, aValue: TmXmlString); override;

    function _GetAttribute(const aName: TmXmlString): TmXmlString; overload; override;
    function _GetAttribute(const aName: TmXmlString; const aDefault: TmXmlString): TmXmlString; overload; override;
    procedure _SetDateTimeAttribute(const aName : TmXmlString; const aValue: TDateTime); override;
    function _GetDateTimeAttribute(const aName: TmXmlString): TDateTime; overload; override;
    function _GetDateTimeAttribute(const aName: TmXmlString; const aDefault : TDateTime): TDateTime; overload; override;
    procedure _SetDateAttribute(const aName : TmXmlString; const aValue: TDateTime); override;
    function _GetDateAttribute(const aName: TmXmlString): TDateTime; overload; override;
    function _GetDateAttribute(const aName: TmXmlString; const aDefault : TDateTime): TDateTime; overload; override;
    procedure _SetFloatAttribute(const aName : TmXmlString; const aValue : double); override;
    function _GetFloatAttribute(const aName: TmXmlString): double; overload; override;
    function _GetFloatAttribute(const aName: TmXmlString; const aDefault : double): double; overload; override;
    procedure _SetIntegerAttribute(const aName : TmXmlString; const aValue: integer); override;
    function _GetIntegerAttribute(const aName: TmXmlString): integer; overload; override;
    function _GetIntegerAttribute(const aName: TmXmlString; const aDefault : integer): integer; overload; override;

    function _HasValue: boolean; override;
    procedure _SetValue(const aValue: TmXMLString); override;
    function _GetValue: TmXMLString; override;
    procedure _SetDateTimeValue(const aValue : TDateTime); override;
    function _GetDateTimeValue: TDateTime; override;
    procedure _SetDateValue(const aValue : TDateTime); override;
    function _GetDateValue: TDateTime; override;
    procedure _SetFloatValue(const aValue : double); override;
    function _GetFloatValue: double; override;
    procedure _SetIntegerValue(const aValue : integer); override;
    function _GetIntegerValue: integer; override;

    procedure _SetOwner(aOwner : TObject); override;
  end;

  { TImpl_oxml_mXmlDocument }

  TImpl_oxml_mXmlDocument = class (TImpl_mXmlDocument)
  private
    FXML: IXMLDocument;
    FRoot : PXMLNode;
    FRootElement : TmXmlElement;
    FGarbage : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function _RootElement: TmXmlElement; override;
    function _CreateRootElement(const aName: string): TmXmlElement; override;
    procedure _Clear; override;
    procedure _SaveToStream(aStream: TStream); override;
    procedure _LoadFromStream(aStream: TStream); override;
    procedure _SaveToFile(const aFileName: string); override;
    procedure _LoadFromFile(const aFileName: string); override;
    procedure _SaveToFileEncrypted (const aFileName : string; const aPassword : String); override;
    procedure _LoadFromFileEncrypted (const aFileName : string; const aPassword : String); override;
  end;

  TImpl_oxml_mXmlElementCursor = class (TImpl_mXmlElementCursor)
  private
    FList : TObjectList;
    FGarbage : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure _SetParent(aParent : TImpl_mXmlElement; aFilter : TmXMLString); override;
    function _GetElement(I : integer) : TmXmlElement; override;
    function _Count : integer; override;
  end;

  TImpl_Factory_oxml = class (TImpl_Factory)
  public
    function GetTImpl_mXmlElement : TImpl_mXmlElement; override;
    function GetTImpl_mXmlDocument : TImpl_mXmlDocument; override;
    function GetTImpl_mXmlElementCursor : TImpl_mXmlElementCursor; override;
  end;


implementation

uses
  BlowFish,
  mISOTime, mUtility, mMathUtility;

{ TImpl_oxml_mXmlElement }

constructor TImpl_oxml_mXmlElement.Create;
begin
  FNode := nil;
  FGarbage := TObjectList.Create(true);

  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.DecimalSeparator:= '.';
  FFormatSettings.CurrencyString:= '€';
  FFormatSettings.ThousandSeparator:= ',';
end;

destructor TImpl_oxml_mXmlElement.Destroy;
begin
  FGarbage.Free;
  inherited Destroy;
end;

procedure TImpl_oxml_mXmlElement.RaiseMissingAttributeException (const Name :TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_AttributeNotFound, [Name]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateTimeAttributeException (const Name, Value :TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongFloatAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongFloatAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongIntegerAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongIntegerAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseMissingValueException(const aNodeName : TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_ValueNotFound, [aNodeName]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateTimeValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateTimeValue, [aNodeName, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateValue, [aNodeName, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongFloatValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongFloatValue, [aNodeName, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongIntegerValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongIntegerAttribute, [aNodeName, Value]));
end;

function TImpl_oxml_mXmlElement._AddElement(const aName: TmXMLString): TmXmlElement;
var
  NewNode : PXMLNode;
  tmp : Tmxml_oxml_PointerShell;
begin
  NewNode := Self.FNode^.AddChild(aName);
  tmp := Tmxml_oxml_PointerShell.Create(NewNode);
  FGarbage.Add(tmp);
  Result := TmXmlElement.Create(tmp);
  FGarbage.Add(Result);
end;

function TImpl_oxml_mXmlElement._GetAttribute(const aName: TmXmlString;
  const aDefault: TmXmlString): TmXmlString;
begin
  Result := Self.FNode^.GetAttributeDef(aName, aDefault);
end;

function TImpl_oxml_mXmlElement._GetDateTimeAttribute(const aName: TmXmlString): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_oxml_mXmlElement._GetDateTimeAttribute(const aName: TmXmlString; const aDefault: TDateTime): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

procedure TImpl_oxml_mXmlElement._SetDateAttribute(const aName: TmXmlString; const aValue: TDateTime);
begin
  FNode^.SetAttribute(aName, mISOTime.ISODateToStr(aValue));
end;

function TImpl_oxml_mXmlElement._GetDateAttribute(const aName: TmXmlString): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_oxml_mXmlElement._GetDateAttribute(const aName: TmXmlString; const aDefault: TDateTime): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

procedure TImpl_oxml_mXmlElement._SetFloatAttribute(const aName: TmXmlString; const aValue: double);
begin
  FNode^.SetAttribute(aName, FloatToStr(aValue, FFormatSettings));
end;

function TImpl_oxml_mXmlElement._GetFloatAttribute(const aName: TmXmlString): double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_oxml_mXmlElement._GetFloatAttribute(const aName: TmXmlString; const aDefault: double): double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

function TImpl_oxml_mXmlElement._GetIntegerAttribute(const aName: TmXmlString): integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_oxml_mXmlElement._GetIntegerAttribute(const aName: TmXmlString; const aDefault: integer): integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode^.HasAttribute(aName) then
  begin
    tmp := FNode^.GetAttribute(aName);
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

function TImpl_oxml_mXmlElement._HasValue: boolean;
begin
  Result := (FNode^.Text <> '');
end;

procedure TImpl_oxml_mXmlElement._SetValue(const aValue: TmXMLString);
begin
  FNode^.Text:= aValue;
end;

function TImpl_oxml_mXmlElement._GetValue: TmXMLString;
begin
  Result := FNode^.Text;
end;

procedure TImpl_oxml_mXmlElement._SetDateTimeValue(const aValue: TDateTime);
begin
  FNode^.Text := mISOTime.ISODateTimeToStr(aValue);
end;

function TImpl_oxml_mXmlElement._GetDateTimeValue: TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.Text <>'' then
  begin
    tmp := FNode^.Text;
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

procedure TImpl_oxml_mXmlElement._SetDateValue(const aValue: TDateTime);
begin
  FNode^.Text := mISOTime.ISODateToStr(aValue);
end;

function TImpl_oxml_mXmlElement._GetDateValue: TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.Text <> '' then
  begin
    tmp := FNode^.Text;
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

procedure TImpl_oxml_mXmlElement._SetFloatValue(const aValue: double);
begin
  FNode^.Text := FloatToStr(aValue, FFormatSettings);
end;

function TImpl_oxml_mXmlElement._GetFloatValue: double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode^.Text <> '' then
  begin
    tmp := FNode^.Text;
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

procedure TImpl_oxml_mXmlElement._SetIntegerValue(const aValue: integer);
begin
  FNode^.Text := IntToStr(aValue);
end;

function TImpl_oxml_mXmlElement._GetIntegerValue: integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode^.Text <> '' then
  begin
    tmp := FNode^.Text;
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

function TImpl_oxml_mXmlElement._GetAttribute(const aName: TmXmlString): TmXmlString;
begin
  if not Self.FNode^.HasAttribute(aName) then
    RaiseMissingAttributeException(aName);
  Result := Self.FNode^.GetAttribute(aName);
end;

function TImpl_oxml_mXmlElement._HasAttribute(const aName: TmXMLString): boolean;
begin
  Result := Self.FNode^.HasAttribute(aName);
end;

procedure TImpl_oxml_mXmlElement._DeleteAttribute(const aName: TmXMLString);
begin
  if FNode^.HasAttribute(aName) then
    FNode^.DeleteAttribute(aName);
end;


procedure TImpl_oxml_mXmlElement._SetAttribute(const aName, aValue: TmXmlString);
begin
  FNode^.SetAttribute(aName, aValue);
end;

procedure TImpl_oxml_mXmlElement._SetDateTimeAttribute(const aName: TmXmlString; const aValue : TDateTime);
begin
  FNode^.SetAttribute(aName, mISOTime.ISODateTimeToStr(aValue));
end;

procedure TImpl_oxml_mXmlElement._SetIntegerAttribute(const aName: TmXmlString; const aValue: integer);
begin
  FNode^.AddAttribute(aName, IntToStr(aValue));
end;

procedure TImpl_oxml_mXmlElement._SetOwner(aOwner: TObject);
begin
  FNode := PXMLNode(Tmxml_oxml_PointerShell(aOwner).MyPointer);
end;

{ TImpl_oxml_mXmlDocument }

constructor TImpl_oxml_mXmlDocument.Create;
begin
  FXML := CreateXMLDoc('', true);
  FXML.WriterSettings.WriteBOM := False;
  FRoot := nil;
  FRootElement := nil;
  FGarbage := TObjectList.Create(true);
end;

destructor TImpl_oxml_mXmlDocument.Destroy;
begin
  Self._Clear;
  FreeAndNil(FRootElement);
  FGarbage.Free;
  inherited;
end;

procedure TImpl_oxml_mXmlDocument._Clear;
begin
  FXML.Clear(true);
end;

function TImpl_oxml_mXmlDocument._CreateRootElement(const aName: string): TmXmlElement;
var
  tmp : Tmxml_oxml_PointerShell;
begin
  assert (not Assigned(FRoot), 'XML RootElement already assigned!');
  FRoot := FXML.AddChild(aName);
  tmp := Tmxml_oxml_PointerShell.Create(FRoot);
  FGarbage.Add(tmp);
  Result := TmXmlElement.Create(tmp);
  FRootElement := Result;
end;

procedure TImpl_oxml_mXmlDocument._LoadFromFile(const aFileName: string);
begin
  if not FXML.LoadFromFile(aFileName) then
    FXML.GetParseError.RaiseException;

  if Assigned(FRootElement) then
    FRootElement.Free;
  FRootElement := nil;
  FRoot := FXML.DocumentElement;
end;

procedure TImpl_oxml_mXmlDocument._SaveToFileEncrypted(const aFileName: string; const aPassword: String);
var
  stream : TMemoryStream;
  fileStream : TFileStream;
  en: TBlowFishEncryptStream;
  buf : TBytes;
  bytesRead : integer;
begin
  stream := TMemoryStream.Create;
  fileStream := TFileStream.Create(aFilename, fmCreate or fmOpenReadWrite);
  en:= TBlowFishEncryptStream.Create(aPassword, fileStream);
  try
    Self._SaveToStream(stream);
    stream.Position:= 0;

    SetLength(buf, 1000);
    bytesRead := stream.Read(buf[0], 1000);
    while bytesRead > 0 do
    begin
      en.Write(buf[0], bytesRead);
      bytesRead := stream.Read(buf[0], 1000);
    end;
  finally
    en.Free;
    fileStream.Free;
    stream.Free;
  end;
end;

procedure TImpl_oxml_mXmlDocument._LoadFromFileEncrypted(const aFileName: string; const aPassword: String);
var
  de: TBlowFishDeCryptStream;
  fileStream: TFileStream;
  stream : TMemoryStream;
  buf : TBytes;
  bytesRead : integer;
begin
  stream := TMemoryStream.Create;
  fileStream := TFileStream.Create(aFilename, fmOpenRead);
  try
    de := TBlowFishDecryptStream.Create(aPassword, fileStream);
    try
      stream.SetSize(fileStream.Size);
      SetLength(buf, 1000);
      bytesRead := de.Read(buf[0], 1000);
      while bytesRead > 0 do
      begin
        stream.Write(buf[0], bytesRead);
        bytesRead := de.Read(buf[0], 1000);
      end;
    finally
      de.Free;
    end;
    stream.Position:= 0;
    Self._LoadFromStream(stream);
  finally
    fileStream.Free;
    stream.Free;
  end;

end;

procedure TImpl_oxml_mXmlDocument._LoadFromStream(aStream: TStream);
begin
  FXML.LoadFromStream(aStream);
  if Assigned(FRootElement) then
    FRootElement.Free;
  FRootElement := nil;
  FRoot := FXML.DocumentElement;
end;

function TImpl_oxml_mXmlDocument._RootElement: TmXmlElement;
var
  tmp : Tmxml_oxml_PointerShell;
begin
  if Assigned(FRoot) and (not Assigned(FRootElement)) then
  begin
    tmp := Tmxml_oxml_PointerShell.Create(FRoot);
    FGarbage.Add(tmp);
    FRootElement := TmXmlElement.Create(tmp);
  end;
  Result := FRootElement;
end;

procedure TImpl_oxml_mXmlDocument._SaveToFile(const aFileName: string);
begin
  FXML.SaveToFile(aFilename);
end;

procedure TImpl_oxml_mXmlDocument._SaveToStream(aStream: TStream);
begin
  FXML.SaveToStream(aStream);
end;

{ Tmxml_oxml_PointerShell }

constructor Tmxml_oxml_PointerShell.Create(aPointer: pointer);
begin
  MyPointer := aPointer;
end;

{ TImpl_oxml_mXmlElementCursor }

constructor TImpl_oxml_mXmlElementCursor.Create;
begin
  FList := TObjectList.Create(true);
  FGarbage := TObjectList.Create(true);
end;

destructor TImpl_oxml_mXmlElementCursor.Destroy;
begin
  FList.Free;
  FGarbage.Free;
  inherited;
end;

function TImpl_oxml_mXmlElementCursor._Count: integer;
begin
  Result := FList.Count;
end;

function TImpl_oxml_mXmlElementCursor._GetElement(I: integer): TmXmlElement;
begin
  Result := FList.Items[i] as TmXmlElement;
end;

procedure TImpl_oxml_mXmlElementCursor._SetParent(aParent: TImpl_mXmlElement; aFilter : TmXMLString);
var
  ParentNode : PXMLNode;
  i : integer;
  NewElement : TmXmlElement;
  tmp : Tmxml_oxml_PointerShell;
begin
  ParentNode := (aParent as TImpl_oxml_mXmlElement).FNode;
  if CompareText(aFilter, '') = 0 then
  begin
    for i := 0 to ParentNode^.ChildCount -1 do
    begin
      tmp := Tmxml_oxml_PointerShell.Create(ParentNode^.ChildNodes[i]);
      FGarbage.Add(tmp);
      NewElement := TmXmlElement.Create(tmp);
      FList.Add(NewElement);
    end;
  end
  else
  begin
    for i := 0 to ParentNode^.ChildCount -1 do
    begin
      if CompareText(aFilter, ParentNode^.ChildNodes[i]^.NodeName) = 0 then
      begin
        tmp := Tmxml_oxml_PointerShell.Create(ParentNode^.ChildNodes[i]);
        FGarbage.Add(tmp);
        NewElement := TmXmlElement.Create(tmp);
        FList.Add(NewElement);
      end;
    end;
  end;
end;

{ TImpl_Factory }

function TImpl_Factory_oxml.GetTImpl_mXmlDocument: TImpl_mXmlDocument;
begin
  Result := TImpl_oxml_mXmlDocument.Create;
end;

function TImpl_Factory_oxml.GetTImpl_mXmlElement: TImpl_mXmlElement;
begin
  Result := TImpl_oxml_mXmlElement.Create;
end;

function TImpl_Factory_oxml.GetTImpl_mXmlElementCursor: TImpl_mXmlElementCursor;
begin
  Result := TImpl_oxml_mXmlElementCursor.Create;
end;

end.
