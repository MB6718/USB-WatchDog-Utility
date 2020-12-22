{*******************************************************}
{                                                       }
{                       Log4Pascal                      }
{        https://github.com/martinusso/log4pascal       }
{                                                       }
{             This software is open source,             }
{       licensed under the The MIT License (MIT).       }
{                                                       }
{           modified version by MB6718 2020             }
{                                                       }
{*******************************************************}

unit Log4Pascal;

interface

type
  TLogTypes = (ltTrace, ltDebug, ltInfo, ltWarning, ltError, ltFatal, ltHead);

  { TLogger }

  TLogger = class
  private
    FFileName: string;
    FIsInit: Boolean;
    FOutFile: TextFile;
    FQuietMode: Boolean;
    FHeadMode: Boolean;
    FQuietTypes: set of TLogTypes;
    procedure Initialize;
    procedure CreateFoldersIfNecessary;
    procedure Finalize;
    procedure Write(const Msg: string);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    property FileName: string read FFileName;

    procedure SetQuietMode;
    procedure DisableTraceLog;
    procedure DisableDebugLog;
    procedure DisableInfoLog;
    procedure DisableWarningLog;
    procedure DisableErrorLog;
    procedure DisableFatalLog;
    procedure DisableHeadlLog;

    procedure SetNoisyMode;
    procedure EnableTraceLog;
    procedure EnableDebugLog;
    procedure EnableInfoLog;
    procedure EnableWarningLog;
    procedure EnableErrorLog;
    procedure EnableFatalLog;
    procedure EnableHeadLog;

    procedure Clear;

    procedure Trace(const Msg: String);
    procedure Debug(const Msg: String);
    procedure Info(const Msg: String);
    procedure Warning(const Msg: String);
    procedure Error(const Msg: String);
    procedure Fatal(const Msg: String);
    procedure Head(const Msg: String);
  end;

var
  Logger: TLogger;

implementation

uses
  Forms,
  SysUtils,
  Windows;

const
  FORMAT_LOG = '%s %s';
  PREFIX_HEAD = '===========';
  PREFIX_TRACE = 'TRACE';
  PREFIX_DEBUG = 'DEBUG';
  PREFIX_INFO  = 'INFO ';
  PREFIX_WARN  = 'WARN ';
  PREFIX_ERROR = 'ERROR';
  PREFIX_FATAL = 'FATAL';

{ TLogger }

procedure TLogger.Clear;
begin
  if not FileExists(FFileName) then
    Exit;

  if FIsInit then
    CloseFile(FOutFile);

  SysUtils.DeleteFile(FFileName);

  FIsInit := False;
end;

constructor TLogger.Create(const FileName: string);
begin
  FFileName := FileName;
  FIsInit := False;
  Self.SetNoisyMode;
  FQuietTypes := [];
end;
 
procedure TLogger.CreateFoldersIfNecessary;
var
  FilePath: string;
  FullApplicationPath: string;
begin
  FilePath := ExtractFilePath(FFileName);

  if Pos(':', FilePath) > 0 then
    ForceDirectories(FilePath)
  else begin
    FullApplicationPath := ExtractFilePath(Application.ExeName);
    ForceDirectories(IncludeTrailingPathDelimiter(FullApplicationPath) + FilePath);
  end;
end;

procedure TLogger.Debug(const Msg: String);
begin
  {{$WARN SYMBOL_PLATFORM OFF}
  if DebugHook = 0 then
    Exit;
  {$WARN SYMBOL_PLATFORM ON}}

  if not (ltDebug in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_DEBUG, Msg]));
end;

destructor TLogger.Destroy;
begin
  Self.Finalize;
  inherited;
end;
 
procedure TLogger.DisableDebugLog;
begin
  Include(FQuietTypes, ltDebug);
end;

procedure TLogger.DisableErrorLog;
begin
  Include(FQuietTypes, ltError);
end;

procedure TLogger.DisableFatalLog;
begin
  Include(FQuietTypes, ltFatal);
end;

procedure TLogger.DisableHeadlLog;
begin
  Include(FQuietTypes, ltHead);
end;

procedure TLogger.DisableInfoLog;
begin
  Include(FQuietTypes, ltInfo);
end;

procedure TLogger.DisableTraceLog;
begin
  Include(FQuietTypes, ltTrace);
end;

procedure TLogger.DisableWarningLog;
begin
  Include(FQuietTypes, ltWarning);
end;

procedure TLogger.EnableDebugLog;
begin
  Exclude(FQuietTypes, ltDebug);
end;

procedure TLogger.EnableErrorLog;
begin
  Exclude(FQuietTypes, ltError);
end;

procedure TLogger.EnableFatalLog;
begin
  Exclude(FQuietTypes, ltFatal);
end;

procedure TLogger.EnableHeadLog;
begin
  Exclude(FQuietTypes, ltHead);
end;

procedure TLogger.EnableInfoLog;
begin
  Exclude(FQuietTypes, ltInfo);
end;

procedure TLogger.EnableTraceLog;
begin
  Exclude(FQuietTypes, ltTrace);
end;

procedure TLogger.EnableWarningLog;
begin
  Exclude(FQuietTypes, ltWarning);
end;

procedure TLogger.Error(const Msg: String);
begin
  if not (ltError in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_ERROR, Msg]));
end;

procedure TLogger.Fatal(const Msg: String);
begin
  if not (ltFatal in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_FATAL, Msg]));
end;

procedure TLogger.Head(const Msg: String);
begin
  if not (ltHead in FQuietTypes) then begin
    FHeadMode := True;
    Self.Write(Format(FORMAT_LOG, [PREFIX_HEAD, Msg]));
  end;
end;

procedure TLogger.Finalize;
begin
  if (FIsInit and (not FQuietMode)) then
    CloseFile(FOutFile);

  FIsInit := False;
  FHeadMode := False;
end;
 
procedure TLogger.Initialize;
begin
  if FIsInit then
    CloseFile(FOutFile);

  if not FQuietMode then
  begin
    Self.CreateFoldersIfNecessary;
    
    AssignFile(FOutFile, FFileName);
    if not FileExists(FFileName) then
      Rewrite(FOutFile)
    else
      Append(FOutFile);
  end;

  FIsInit := True;
end;
 
procedure TLogger.Info(const Msg: String);
begin
  if not (ltInfo in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_INFO, Msg]));
end;
 
procedure TLogger.SetNoisyMode;
begin
  FQuietMode := False;
end;
 
procedure TLogger.SetQuietMode;
begin
  FQuietMode := True;
end;
 
procedure TLogger.Trace(const Msg: String);
begin
  if not (ltTrace in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_TRACE, Msg]));
end;

procedure TLogger.Warning(const Msg: String);
begin
  if not (ltWarning in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_WARN, Msg]));
end;
 
procedure TLogger.Write(const Msg: String);
const
  FORMAT_DATETIME_DEFAULT = 'yyyy-mm-dd hh:nn:ss';
begin
  if FQuietMode then
    Exit;

  Self.Initialize;
  try
    if FIsInit then
      if FHeadMode then
        Writeln(FOutFile, Format('%s  [%s] %s',
          [Msg, FormatDateTime(FORMAT_DATETIME_DEFAULT, Now), PREFIX_HEAD]))
      else
        Writeln(FOutFile, Format('[%s]  %s',
          [FormatDateTime(FORMAT_DATETIME_DEFAULT, Now), Msg]));
  finally
    Self.Finalize;
  end;
end;

initialization
  //Logger := TLogger.Create('Log.txt');

finalization
  Logger.Free;

end.
