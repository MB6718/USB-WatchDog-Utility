unit AppConfigs;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, IniFiles, Dialogs;

const
  main_section = 'Main';
  app_section = 'App';
  window_section = 'Window';
  info_section = 'Info';

var
  INI: TINIFile;

  ResetTimeOut,
  ResetMode,
  USBPwrMode,
  PingTimeOut,
  WinPosX,
  WinPosY: Integer;

  NetAddress,
  WinStartState,
  DefaultPort: string;

  AutoChkUpdates,
  NetMonitoring,
  LaunchWithOS,
  MinimizeOnClose,
  inSysTray,
  UseLog,
  AutoConnect: boolean;

{ Описание прототипов процедур и функций }
procedure ReadAppConfigs(const ConfigFile: string);
procedure WriteAppConfigs(const ConfigFile: string);

implementation

procedure ReadAppConfigs(const ConfigFile: string);
begin
  INI:=TINIFile.Create(ExtractFilePath(ParamStr(0)) + ConfigFile);
  try
    { MainSection }
    NetMonitoring:=INI.ReadBool(main_section, 'NetMonitoring', False);
    NetAddress:=INI.ReadString(main_section, 'NetAddress', '');
    PingTimeOut:=INI.ReadInteger(main_section, 'PingTimeOut', 1000);
    ResetTimeOut:=INI.ReadInteger(main_section, 'ResetTimeOut', 180); // sec
    ResetMode:=INI.ReadInteger(main_section, 'ResetMode', 1); // A0 - 0; A1 - 1
    USBPwrMode:=INI.ReadInteger(main_section, 'USBPwrMode', 0); // USB off - 0; USB on - 1

    { AppSection }
    DefaultPort:=INI.ReadString(app_section, 'DefaultPort', '');
    AutoConnect:=INI.ReadBool(app_section, 'AutoConnect', False);
    UseLog:=INI.ReadBool(app_section, 'UseLog', False);
    MinimizeOnClose:=INI.ReadBool(app_section, 'MinimizeOnClose', False);
    inSysTray:=INI.ReadBool(app_section, 'inSysTray', False);
    LaunchWithOS:=INI.ReadBool(app_section, 'LaunchWithOS', False);
    AutoChkUpdates:=INI.ReadBool(app_section, 'AutoChkUpdates', False);

    { WindowSection }
    WinStartState:=INI.ReadString(window_section, 'WindowStartState', 'normal');
    WinPosX:=INI.ReadInteger(window_section, 'WindowPositionX', -1);
    WinPosY:=INI.ReadInteger(window_section, 'WindowPositionY', -1);
  finally
    INI.Free;
  end;
end;

procedure WriteAppConfigs(const ConfigFile: string);
begin
  INI:=TINIFile.Create(ExtractFilePath(ParamStr(0)) + ConfigFile);
  try
    { MainSection }
    INI.WriteBool(main_section, 'NetMonitoring', NetMonitoring);
    INI.WriteString(main_section, 'NetAddress', NetAddress);
    INI.WriteInteger(main_section, 'PingTimeOut', PingTimeOut);
    INI.WriteInteger(main_section, 'ResetTimeout', ResetTimeOut);
    INI.WriteInteger(main_section, 'ResetMode', ResetMode);
    INI.WriteInteger(main_section, 'USBPwrMode', USBPwrMode);

    { AppSection }
    INI.WriteString(app_section, 'DefaultPort', DefaultPort);
    INI.WriteBool(app_section, 'AutoConnect', AutoConnect);
    INI.WriteBool(app_section, 'UseLog', UseLog);
    INI.WriteBool(app_section, 'MinimizeOnClose', MinimizeOnClose);
    INI.WriteBool(app_section, 'inSysTray', inSysTray);
    INI.WriteBool(app_section, 'LaunchWithOS', LaunchWithOS);
    INI.WriteBool(app_section, 'AutoChkUpdates', AutoChkUpdates);

    { WindowSection }
    INI.WriteString(window_section, 'WindowStartState', WinStartState);
    INI.WriteInteger(window_section, 'WindowPositionX', WinPosX);
    INI.WriteInteger(window_section, 'WindowPositionY', WinPosY);
  finally
    INI.Free;
  end;
end;

end.

