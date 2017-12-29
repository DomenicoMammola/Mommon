unit mDataProvidersCache;


interface

uses
  Classes, contnrs;

type
  TDataProviderDoGetData = procedure of object;

  { TDataProviderCacheItem }

  TDataProviderCacheItem = class
  strict private
    FId : string;
    FCanBeCached : boolean;
    FDoGetData : TDataProviderDoGetData;
  public
    constructor Create;

    property Id : string read FId write FId;
    property CanBeCached : boolean read FCanBeCached write FCanBeCached;
    property DoGetData : TDataProviderDoGetData read FDoGetData write FDoGetData;
  end;

  TDataProviderCache = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Add : TDataProviderCacheItem;
  end;


implementation

{ TDataProviderCache }

constructor TDataProviderCache.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TDataProviderCache.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TDataProviderCache.Add: TDataProviderCacheItem;
begin
  Result := TDataProviderCacheItem.Create;
  FList.Add(Result);
end;

{ TDataProviderCacheItem }

constructor TDataProviderCacheItem.Create;
begin
  FId := '';
  FCanBeCached:= true;
  FDoGetData:= nil;
end;

end.
