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

  WinPosX,
  WinPosY: Integer;

  WinStartState,
  DefaultPort: string;

  MinimizeOnClose,
  inSysTray,
  UseLog,
  AutoConnect: boolean;

{ Описание прототипов процедур и функций }
procedure ReadAppConfigs(ConfigFile: string);
procedure WriteAppConfigs(ConfigFile: string);

implementation

procedure ReadAppConfigs(ConfigFile: string);
begin
  INI:=TINIFile.Create(ExtractFilePath(ParamStr(0)) + ConfigFile);
  try
    { AppSection }
    DefaultPort:=INI.ReadString(app_section, 'DefaultPort', '');
    AutoConnect:=INI.ReadBool(app_section, 'AutoConnect', False);
    UseLog:=INI.ReadBool(app_section, 'UseLog', False);
    MinimizeOnClose:=INI.ReadBool(app_section, 'MinimizeOnClose', False);
    inSysTray:=INI.ReadBool(app_section, 'inSysTray', False);

    { WindowSection }
    WinStartState:=INI.ReadString(window_section, 'WindowStartState', 'normal');
    WinPosX:=INI.ReadInteger(window_section, 'WindowPositionX', -1);
    WinPosY:=INI.ReadInteger(window_section, 'WindowPositionY', -1);
  finally
    INI.Free;
  end;
end;

procedure WriteAppConfigs(ConfigFile: string);
begin
  INI:=TINIFile.Create(ExtractFilePath(ParamStr(0)) + ConfigFile);
  try
    { AppSection }
    INI.WriteString(app_section, 'DefaultPort', DefaultPort);
    INI.WriteBool(app_section, 'AutoConnect', AutoConnect);
    INI.WriteBool(app_section, 'UseLog', UseLog);
    INI.WriteBool(app_section, 'MinimizeOnClose', MinimizeOnClose);
    INI.WriteBool(app_section, 'inSysTray', inSysTray);

    { WindowSection }
    INI.WriteString(window_section, 'WindowStartState', WinStartState);
    INI.WriteInteger(window_section, 'WindowPositionX', WinPosX);
    INI.WriteInteger(window_section, 'WindowPositionY', WinPosY);
  finally
    INI.Free;
  end;
end;

end.

