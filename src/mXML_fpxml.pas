unit mXML_fpxml;

interface

uses
  Classes, laz2_DOM, SysUtils, Contnrs,
  mXML;

resourcestring
 mXML_fpxml_AttributeNotFound = 'XML attribute %s not found.';
 mXML_fpxml_WrongDateTimeAttribute = 'XML attribute %s has value %s which is not in ISO date-time format.';
 mXML_fpxml_WrongDateAttribute = 'XML attribute %s has value $s which is not in ISO date format.';
 mXML_fpxml_WrongFloatAttribute = 'XML attribute %s has value %s that is not a valid float number.';
 mXML_fpxml_WrongIntegerAttribute = 'XML attribute %s has value %s that is not a valid integer number.';

 mXML_fpxml_ValueNotFound = 'XML element %s has no value.';
 mXML_fpxml_WrongDateTimeValue = 'XML element %s has value %s which is not in ISO date-time format.';
 mXML_fpxml_WrongDateValue = 'XML element %s has value $s which is not in ISO date format.';
 mXML_fpxml_WrongFloatValue = 'XML element %s has value %s that is not a valid float number.';
 mXML_fpxml_WrongIntegerValue = 'XML element %s has value %s that is not a valid integer number.';

type

  { TImpl_fpxml_mXmlElement }

  TImpl_fpxml_mXmlElement = class (TImpl_mXmlElement)
  strict private
    FFormatSettings : TFormatSettings;
    FGarbage : TObjectList;
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
  private
    FNode : TDOMElement;
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
    procedure _SetDateTimeAttribute(const aName : TmXmlString; const aValue : TDateTime); override;
    function _GetDateTimeAttribute(const aName: TmXmlString): TDateTime; overload; override;
    function _GetDateTimeAttribute(const aName: TmXmlString; const aDefault : TDateTime): TDateTime; overload; override;
    procedure _SetDateAttribute(const aName : TmXmlString; const aValue : TDateTime); override;
    function _GetDateAttribute(const aName: TmXmlString): TDateTime; overload; override;
    function _GetDateAttribute(const aName: TmXmlString; const aDefault : TDateTime): TDateTime; overload; override;
    procedure _SetFloatAttribute(const aName : TmXmlString; const aValue : double); override;
    function _GetFloatAttribute(const aName: TmXmlString): double; overload; override;
    function _GetFloatAttribute(const aName: TmXmlString; const aDefault : double): double; overload; override;
    procedure _SetIntegerAttribute(const aName : TmXmlString; const aValue: integer); override;
    function _GetIntegerAttribute(const aName: TmXmlString): integer; overload; override;
    function _GetIntegerAttribute(const aName: TmXmlString; const aDefault : integer): integer; overload; override;
    function _HasValue: boolean; override;
    procedure _SetValue(const aValue: TmXMLString); overload; override;
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


  { TImpl_fpxml_mXmlDocument }

  TImpl_fpxml_mXmlDocument = class (TImpl_mXmlDocument)
  strict private
    FXMLDocument : TXMLDocument;
    FRoot : TDOMElement;
    FRootElement : TmXmlElement;
    procedure CheckXMLDocument;
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

  { TImpl_fpxml_mXmlElementCursor }

  TImpl_fpxml_mXmlElementCursor = class (TImpl_mXmlElementCursor)
  strict private
    FList : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure _SetParent(aParent : TImpl_mXmlElement; aFilter : TmXMLString); override;
    function _GetElement(I : integer) : TmXmlElement; override;
    function _Count : integer; override;
  end;

  { TImpl_Factory_fpxml }

  TImpl_Factory_fpxml = class (TImpl_Factory)
  public
    function GetTImpl_mXmlElement : TImpl_mXmlElement; override;
    function GetTImpl_mXmlDocument : TImpl_mXmlDocument; override;
    function GetTImpl_mXmlElementCursor : TImpl_mXmlElementCursor; override;
  end;

implementation

uses
  laz2_XMLRead, laz2_XMLWrite, BlowFish,
  mISOTime, mMathUtility;

{ TImpl_Factory_fpxml }

function TImpl_Factory_fpxml.GetTImpl_mXmlElement: TImpl_mXmlElement;
begin
  Result := TImpl_fpxml_mXmlElement.Create;
end;

function TImpl_Factory_fpxml.GetTImpl_mXmlDocument: TImpl_mXmlDocument;
begin
  Result := TImpl_fpxml_mXmlDocument.Create;
end;

function TImpl_Factory_fpxml.GetTImpl_mXmlElementCursor: TImpl_mXmlElementCursor;
begin
  Result := TImpl_fpxml_mXmlElementCursor.Create;
end;


{ TImpl_fpxml_mXmlElementCursor }

constructor TImpl_fpxml_mXmlElementCursor.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TImpl_fpxml_mXmlElementCursor.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TImpl_fpxml_mXmlElementCursor._SetParent(aParent: TImpl_mXmlElement; aFilter: TmXMLString);
var
  parentNode, curChild : TDOMNode;
  i : integer;
  NewElement : TmXmlElement;
begin
  parentNode := (aParent as TImpl_fpxml_mXmlElement).FNode;
  if CompareText(aFilter, '') = 0 then
  begin
    curChild := parentNode.FirstChild;
    while Assigned(curChild) do
    begin
      NewElement := TmXmlElement.Create(curChild);
      FList.Add(NewElement);
      curChild := curChild.NextSibling;
    end;
  end
  else
  begin
    curChild := parentNode.FirstChild;
    while Assigned(curChild) do
    begin
      if CompareText(aFilter, curChild.NodeName) = 0 then
      begin
        NewElement := TmXmlElement.Create(curChild);
        FList.Add(NewElement);
      end;
      curChild := curChild.NextSibling;
    end;
  end;
end;

function TImpl_fpxml_mXmlElementCursor._GetElement(I: integer): TmXmlElement;
begin
  Result := FList.Items[i] as TmXmlElement;
end;

function TImpl_fpxml_mXmlElementCursor._Count: integer;
begin
  Result := FList.Count;
end;

{ TImpl_fpxml_mXmlElement }

constructor TImpl_fpxml_mXmlElement.Create;
begin
  FNode := nil;
  FGarbage := TObjectList.Create(true);

  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.DecimalSeparator:= '.';
  FFormatSettings.CurrencyString:= '€';
  FFormatSettings.ThousandSeparator:= ',';
end;

destructor TImpl_fpxml_mXmlElement.Destroy;
begin
  FGarbage.Free;
  inherited Destroy;
end;

function TImpl_fpxml_mXmlElement._AddElement(const aName: TmXMLString): TmXmlElement;
var
  NewNode : TDOMElement;
begin
  NewNode := FNode.OwnerDocument.CreateElement(aName);
  Self.FNode.AppendChild(NewNode);
  Result := TmXmlElement.Create(NewNode);
  FGarbage.Add(Result);
end;

function TImpl_fpxml_mXmlElement._HasAttribute(const aName: TmXMLString): boolean;
begin
  Result := FNode.hasAttribute(aName);
end;

procedure TImpl_fpxml_mXmlElement._DeleteAttribute(const aName: TmXMLString);
begin
  FNode.RemoveAttribute(aName);
end;

procedure TImpl_fpxml_mXmlElement._SetAttribute(const aName, aValue: TmXmlString);
begin
  FNode.SetAttribute(aName, aValue);
end;

function TImpl_fpxml_mXmlElement._GetAttribute(const aName: TmXmlString): TmXmlString;
begin
  Result := FNode.GetAttribute(aName);
end;

function TImpl_fpxml_mXmlElement._GetAttribute(const aName: TmXmlString; const aDefault: TmXmlString): TmXmlString;
begin
  if FNode.hasAttribute(aName) then
    Result := FNode.GetAttribute(aName)
  else
    Result := aDefault;
end;

procedure TImpl_fpxml_mXmlElement._SetDateTimeAttribute(const aName: TmXmlString; const aValue: TDateTime);
begin
  FNode.SetAttribute(aName, mISOTime.ISODateTimeToStr(aValue));
end;

function TImpl_fpxml_mXmlElement._GetDateTimeAttribute(const aName: TmXmlString): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_fpxml_mXmlElement._GetDateTimeAttribute(const aName: TmXmlString; const aDefault: TDateTime): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

procedure TImpl_fpxml_mXmlElement._SetDateAttribute(const aName: TmXmlString; const aValue: TDateTime);
begin
  FNode.SetAttribute(aName, mISOTime.ISODateToStr(aValue));
end;

function TImpl_fpxml_mXmlElement._GetDateAttribute(const aName: TmXmlString): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_fpxml_mXmlElement._GetDateAttribute(const aName: TmXmlString; const aDefault: TDateTime): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

procedure TImpl_fpxml_mXmlElement._SetFloatAttribute(const aName: TmXmlString; const aValue: double);
begin
  FNode.SetAttribute(aName, FloatToStr(aValue, FFormatSettings));
end;

function TImpl_fpxml_mXmlElement._GetFloatAttribute(const aName: TmXmlString): double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_fpxml_mXmlElement._GetFloatAttribute(const aName: TmXmlString; const aDefault: double): double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

procedure TImpl_fpxml_mXmlElement._SetIntegerAttribute(const aName: TmXmlString; const aValue: integer);
begin
  FNode.SetAttribute(aName, IntToStr(aValue));
end;

function TImpl_fpxml_mXmlElement._GetIntegerAttribute(const aName: TmXmlString): integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerAttributeException(aName, tmp);
  end
  else
    RaiseMissingAttributeException(aName);
end;

function TImpl_fpxml_mXmlElement._GetIntegerAttribute(const aName: TmXmlString; const aDefault: integer): integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode.hasAttribute(aName) then
  begin
    tmp := FNode.GetAttribute(aName);
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerAttributeException(aName, tmp);
  end
  else
    Result := aDefault;
end;

function TImpl_fpxml_mXmlElement._HasValue: boolean;
begin
  Result := FNode.TextContent <> '';
end;

procedure TImpl_fpxml_mXmlElement._SetValue(const aValue: TmXMLString);
begin
  FNode.TextContent:= aValue;
end;

function TImpl_fpxml_mXmlElement._GetValue: TmXMLString;
begin
  Result := FNode.TextContent;
end;

procedure TImpl_fpxml_mXmlElement._SetDateTimeValue(const aValue: TDateTime);
begin
  FNode.TextContent := mISOTime.ISODateTimeToStr(aValue);
end;

function TImpl_fpxml_mXmlElement._GetDateTimeValue: TDateTime;
var
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode.TextContent <>'' then
  begin
    if TryISOStrToDateTime(FNode.TextContent, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeValueException(FNode.NodeName, FNode.TextContent);
  end
  else
    RaiseMissingValueException(FNode.NodeName);
end;

procedure TImpl_fpxml_mXmlElement._SetDateValue(const aValue: TDateTime);
begin
  FNode.TextContent := mISOTime.ISODateToStr(aValue);
end;

function TImpl_fpxml_mXmlElement._GetDateValue: TDateTime;
var
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode.TextContent <> '' then
  begin
    if TryISOStrToDate(FNode.TextContent, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateValueException(FNode.NodeName, FNode.TextContent);
  end
  else
    RaiseMissingValueException(FNode.NodeName);
end;

procedure TImpl_fpxml_mXmlElement._SetFloatValue(const aValue: double);
begin
  FNode.TextContent := FloatToStr(aValue, FFormatSettings);
end;

function TImpl_fpxml_mXmlElement._GetFloatValue: double;
var
  tempValue : double;
begin
  Result := 0;
  if FNode.TextContent <> '' then
  begin
    if TryToConvertToDouble(FNode.TextContent, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatValueException(FNode.NodeName, FNode.TextContent);
  end
  else
    RaiseMissingValueException(FNode.NodeName);
end;

procedure TImpl_fpxml_mXmlElement._SetIntegerValue(const aValue: integer);
begin
  FNode.TextContent := IntToStr(aValue);
end;

function TImpl_fpxml_mXmlElement._GetIntegerValue: integer;
var
  tempValue : integer;
begin
  Result := 0;
  if FNode.TextContent <> '' then
  begin
    if TryToConvertToInteger(FNode.TextContent, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerValueException(FNode.NodeName, FNode.TextContent);
  end
  else
    RaiseMissingValueException(FNode.NodeName);
end;

procedure TImpl_fpxml_mXmlElement._SetOwner(aOwner: TObject);
begin
  FNode := aOwner as TDOMElement;
end;

procedure TImpl_fpxml_mXmlElement.RaiseMissingAttributeException (const Name :TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_AttributeNotFound, [Name]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongDateTimeAttributeException (const Name, Value :TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongDateAttribute, [Name, Value]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongDateAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongDateAttribute, [Name, Value]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongFloatAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongFloatAttribute, [Name, Value]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongIntegerAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongIntegerAttribute, [Name, Value]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseMissingValueException(const aNodeName : TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_ValueNotFound, [aNodeName]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongDateTimeValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongDateTimeValue, [aNodeName, Value]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongDateValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongDateValue, [aNodeName, Value]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongFloatValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongFloatValue, [aNodeName, Value]));
end;

procedure TImpl_fpxml_mXmlElement.RaiseWrongIntegerValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_fpxml_WrongIntegerAttribute, [aNodeName, Value]));
end;


{ TImpl_fpxml_mXmlDocument }

procedure TImpl_fpxml_mXmlDocument.CheckXMLDocument;
begin
  if not Assigned(FXMLDocument) then
    FXMLDocument := TXMLDocument.Create;
end;

constructor TImpl_fpxml_mXmlDocument.Create;
begin
  FXMLDocument := nil;
  FRoot := nil;
  FRootElement := nil;
end;

destructor TImpl_fpxml_mXmlDocument.Destroy;
begin
  FreeAndNil(FXMLDocument);
  FreeAndNil(FRootElement);
  inherited Destroy;
end;

function TImpl_fpxml_mXmlDocument._RootElement: TmXmlElement;
begin
  CheckXMLDocument;
  if Assigned(FRoot) and (not Assigned(FRootElement)) then
    FRootElement := TmXmlElement.Create(FRoot);
  Result := FRootElement;
end;

function TImpl_fpxml_mXmlDocument._CreateRootElement(const aName: string): TmXmlElement;
begin
  assert (not Assigned(FRoot), 'XML RootElement already assigned!');
  CheckXMLDocument;
  FRoot := FXMLDocument.CreateElement(aName);
  FXMLDocument.Appendchild(FRoot);
  Result := TmXmlElement.Create(FRoot);
  FRootElement := Result;
end;

procedure TImpl_fpxml_mXmlDocument._Clear;
begin
  if Assigned(FRoot) and Assigned(FXMLDocument) then
    FXMLDocument.RemoveChild(FRoot);
  FreeAndNil(FRootElement);
end;

procedure TImpl_fpxml_mXmlDocument._SaveToStream(aStream: TStream);
begin
  WriteXMLFile(FXMLDocument, aStream);
end;

procedure TImpl_fpxml_mXmlDocument._LoadFromStream(aStream: TStream);
begin
  FreeAndNil(FXMLDocument);
  FreeAndNil(FRootElement);
  ReadXMLFile(FXMLDocument, aStream);
  FRoot := FXMLDocument.DocumentElement;
end;

procedure TImpl_fpxml_mXmlDocument._SaveToFile(const aFileName: string);
begin
  WriteXMLFile(FXMLDocument, aFileName);
end;

procedure TImpl_fpxml_mXmlDocument._LoadFromFile(const aFileName: string);
begin
  FreeAndNil(FXMLDocument);
  FreeAndNil(FRootElement);
  ReadXMLFile(FXMLDocument, aFileName);
  FRoot := FXMLDocument.DocumentElement;
end;

procedure TImpl_fpxml_mXmlDocument._SaveToFileEncrypted(const aFileName: string; const aPassword: String);
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

procedure TImpl_fpxml_mXmlDocument._LoadFromFileEncrypted(const aFileName: string; const aPassword: String);
var
  de: TBlowFishDeCryptStream;
  fileStream: TFileStream;
  stream : TStringStream;
  buf : TBytes;
  bytesRead : integer;
  list : TStringList;
  i : integer;
begin
  FreeAndNil(FXMLDocument);
  stream := TStringStream.Create('', TEncoding.UTF8);
  fileStream := TFileStream.Create(aFilename, fmOpenRead);
  try
    de := TBlowFishDecryptStream.Create(aPassword, fileStream);
    try
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

    // workaround for double #0#0 chars at the end of the stream
    list := TStringList.Create;
    try
      stream.Position:= 0;
      list.LoadFromStream(stream, TEncoding.UTF8);
      for i := 0 to list.Count - 1 do
        list.Strings[i] := StringReplace(list.Strings[i], #0, '', [rfReplaceAll]);
      stream.Clear;
      list.SaveToStream(stream);
    finally
      list.Free;
    end;
    stream.Position:= 0;
    Self._LoadFromStream(stream);
  finally
    fileStream.Free;
    stream.Free;
  end;
end;

end.
