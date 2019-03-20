unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  mDatabaseConnection,
  mDatabaseConnectionImplOnMySQL56;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnConnetti: TButton;
    BtnQuery: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure BtnConnettiClick(Sender: TObject);
    procedure BtnQueryClick(Sender: TObject);
  private
    function GetConnection : TmDatabaseConnection;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  mDatabaseConnectionClasses;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnConnettiClick(Sender: TObject);
var
  connection : TmDatabaseConnection;
begin
  connection := Self.GetConnection;
  try
    connection.Connect;
    if connection.Connected then
      Memo1.Lines.Add('Connesso')
    else
      Memo1.Lines.Add('Connessione fallita');
    connection.Close;
  finally
    connection.Free;
  end;
end;

procedure TForm1.BtnQueryClick(Sender: TObject);
var
  connection : TmDatabaseConnection;
  query : TmDatabaseQuery;
begin
  connection := Self.GetConnection;
  try
    connection.Connect;
    if connection.Connected then
      Memo1.Lines.Add('Connesso')
    else
    begin
      Memo1.Lines.Add('Connessione fallita');
      exit;
    end;

    query := TmDatabaseQuery.Create;
    try
      query.DatabaseConnection := connection;
      query.SetSQL('SELECT * FROM navi_ocr');
      query.Open;
      while not query.Eof do
        query.Next;
      Memo1.Lines.Add('Trovate ' + IntToStr(query.AsDataset.RecordCount) + ' righe');
      query.Close;
    finally
      query.Free;
    end;


    connection.Close;
  finally
    connection.Free;
  end;
end;

function TForm1.GetConnection: TmDatabaseConnection;
var
  connInfo : TmDatabaseConnectionInfo;
begin
  connInfo := TmDatabaseConnectionInfo.Create;
  connInfo.VendorType:= dvMySQL56;
  connInfo.Server:= '127.0.0.1';
  connInfo.UserName:= 'root';
  connInfo.Password:= '****';
  connInfo.DatabaseName:= 'sp1_base';
  Result := TmDatabaseConnection.Create(connInfo, true);
end;

end.

