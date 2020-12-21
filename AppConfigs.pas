unit AppConfigs;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, IniFiles, Dialogs;

const
  app_section = 'App';

var
  INI: TINIFile;

  DefaultPort: string;
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
  finally
    INI.Free;
  end;
end;

end.

