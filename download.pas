unit Download;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, fphttpclient;

type

  { TfDownload }

  TfDownload = class(TForm)
    ProgressLabel: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FHTTPClient: TFPHTTPClient;
    FromURL: String;
    toFileName: String;
    UpdateAllowed: Boolean;
    function Download(aFrom, aTo: String): Boolean;
    procedure DoOnWriteStream(Sender: TObject; aPosition: LongInt);
    function FormatSize(Size: Int64): String;
    function FormatPercentage(CurrentValue, MaxValue: LongInt): String;
    function GetContentLength(Head: TStringList): LongInt;
    procedure ProgressBar1StepPosition(aPosition: LongInt);
  public
    procedure SetDownload(aFrom, aTo: String);
  end;

var
  fDownload: TfDownload;

implementation

{$R *.lfm}

type

  { TDownloadStream }

  TOnWriteStream = procedure(Sender: TObject; aPosition: LongInt) of object;
  TDownloadStream = class(TStream)
  private
    FOnWriteStream: TOnWriteStream;
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    procedure DoProgress;
  published
    property OnWriteStream: TOnWriteStream read FOnWriteStream write FOnWriteStream;
  end;

{ TDownloadStream }

constructor TDownloadStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream:=AStream;
  FStream.Position:=0;
end;

destructor TDownloadStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TDownloadStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result:=FStream.Read(Buffer, Count);
end;

function TDownloadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result:=FStream.Write(Buffer, Count);
  DoProgress;
end;

function TDownloadStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  Result:=FStream.Seek(Offset, Origin);
end;

procedure TDownloadStream.DoProgress;
begin
  if Assigned(FOnWriteStream) then
    FOnWriteStream(Self, Self.Position);
end;

{ TfDownload }

function TfDownload.Download(aFrom, aTo: String): Boolean;
var
  DS: TDownloadStream;
  S: TStringList;
  size: Integer;
begin
  Result:=True;
  S:=TStringList.Create;
  DS:=TDownloadStream.Create(TFileStream.Create(aTo, fmCreate));
  try
    DS.FOnWriteStream:=@DoOnWriteStream;
    try
      TFPHTTPClient.Head(aFrom, S);
      size:=GetContentLength(S);
      if size > 0 then begin
        with ProgressBar1 do begin
          Position:=0;
          Max:=size;
        end;
        FHTTPClient.HTTPMethod('GET', aFrom, DS, [200]);
      end
      else
        Result:=False;
    except
      on E: Exception do begin
        Result:=False;
        MessageDlg('App download error!', E.Message, mtError, [mbOK], 0);
        UpdateAllowed:=False;
        Self.Close;
      end;
    end;
  finally
    DS.Free;
    S.Free;
  end;
end;

function TfDownload.FormatSize(Size: Int64): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result:=FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result:=FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result:=FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result:=FormatFloat('#,##0.0 GB', Size / GB);
end;

function TfDownload.FormatPercentage(CurrentValue, MaxValue: LongInt): String;
begin
  Result:=FormatFloat('#,##0.0 %', CurrentValue / MaxValue * 100);
end;

function TfDownload.GetContentLength(Head: TStringList): LongInt;
const
  aFindBlank = 'Content-Length:';
var
  index: Integer;
  size, str: String;
begin
  Result:=0;
  Head.Delimiter:='#';
  str:=Head.DelimitedText;
  index:=Pos(aFindBlank, str);
  if index > 0 then begin
    index:=index + aFindBlank.Length;
    size:='';
    while str[index] <> '"' do begin
      size:=size + str[index];
      inc(index);
    end;
    Result:=StrToInt(Trim(size));
  end
end;

procedure TfDownload.ProgressBar1StepPosition(aPosition: LongInt);
begin
  with ProgressBar1 do
    if (aPosition < Max) then begin
      Position:=aPosition + 1;
      Position:=aPosition;
    end
    else begin
      Max:=aPosition + 1;
      Position:=aPosition + 1;
      Max:=aPosition;
    end;
end;

procedure TfDownload.SetDownload(aFrom, aTo: String);
begin
  FromURL:=aFrom;
  toFileName:=aTo;
  UpdateAllowed:=True;
end;

procedure TfDownload.DoOnWriteStream(Sender: TObject; aPosition: LongInt);
var
  MaxValue: Integer;
begin
  MaxValue:=ProgressBar1.Max;
  ProgressLabel.Caption:=('Downloaded: ' + FormatSize(aPosition) + ' ( ' +
    FormatPercentage(aPosition, MaxValue) + ' )');
  ProgressBar1StepPosition(aPosition);
  Application.ProcessMessages;
end;

procedure TfDownload.FormCreate(Sender: TObject);
begin
  FHTTPClient:=TFPHTTPClient.Create(nil);
end;

procedure TfDownload.FormDestroy(Sender: TObject);
begin
  FHTTPClient.Free;
end;

procedure TfDownload.FormShow(Sender: TObject);
begin
  Timer1.Enabled:=True;
end;

procedure TfDownload.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if UpdateAllowed then
    CloseAction:=caNone
  else
    CloseAction:=caFree;
end;

procedure TfDownload.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  if UpdateAllowed and (FromURL <> '') and (toFileName <> '') then
    if Download(FromURL, toFileName) then begin
      UpdateAllowed:=False;
      Self.Close;
    end;
end;

end.

