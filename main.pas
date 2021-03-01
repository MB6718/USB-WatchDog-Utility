unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, Clipbrd, Menus, LazSerial, LazSynaSer,
  RegExpr, windows, registry, pingsend, UniqueInstance, LCLIntf, LCLType,
  ActnList, Log4Pascal, Fpjson, jsonparser, fphttpclient, Types;

type

  { App Types }

  TUpdateVersion = record
    prefix,
    version,
    build,
    date,
    filename: String;
  end;

  TRunState = (RunDisable, RunEnable);
  TSendCommand = (SoftResetCmd, HardResetCmd, PowerOffCmd);

  { TfMain }

  TfMain = class(TForm)
    SoftResetAction: TAction;
    HardResetAction: TAction;
    PowerOffAction: TAction;
    ActionList1: TActionList;
    ButtonsGroupBox: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    ChkAppUpdatesButton: TButton;
    CleanLogButton: TSpeedButton;
    ConnectionGroupBox: TGroupBox;
    CopiedLabel: TLabel;
    DefaultButton: TBitBtn;
    DeviceStatusLabel: TLabel;
    ETHWalletLabel: TLabel;
    BTCWalletLabel: TLabel;
    FirmwareVersionLabel: TLabel;
    HardResetButton: TButton;
    HelpButton: TSpeedButton;
    ImageList1: TImageList;
    IndicatorShape: TShape;
    InfoLabel: TLabel;
    InfoLabel1: TLabel;
    AppUpdateLabel: TLabel;
    LazSerial1: TLazSerial;
    m10Label: TLabel;
    m15Label: TLabel;
    m20Label: TLabel;
    m3Label: TLabel;
    m5Label: TLabel;
    ModesRadioGroup: TRadioGroup;
    NetAddressEdit: TEdit;
    NetMonitoringCheckBox: TCheckBox;
    PanelLabel1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    MainTabSheet: TTabSheet;
    DeviceTabSheet: TTabSheet;
    AppTabSheet: TTabSheet;
    AboutTabSheet: TTabSheet;
    PingGroupBox: TGroupBox;
    PingStatusIndicatorLabel: TLabel;
    PingStatusLabel: TLabel;
    PingTimeoutLabel: TLabel;
    PingTimeoutSecLabel: TLabel;
    PingTimeoutTrackBar: TTrackBar;
    PopupMenu1: TPopupMenu;
    PortSelectorComboBox: TComboBox;
    PowerModeRadioGroup: TRadioGroup;
    PowerOffButton: TButton;
    ReScanButton: TButton;
    s10Label: TLabel;
    SplitterShape: TShape;
    SoftResetButton: TButton;
    StartStopButton: TBitBtn;
    Timer1: TTimer;
    DeviceTimer: TTimer;
    ChkUpdateTimer: TTimer;
    TitleLabel: TLabel;
    TrayIcon1: TTrayIcon;
    TrayMenuItemExit: TMenuItem;
    TrayMenuItemRestore: TMenuItem;
    TrayMenuSep1: TMenuItem;
    TrayPopupMenu: TPopupMenu;
    UniqueInstance1: TUniqueInstance;
    WaitingSecLabel: TLabel;
    WaitingTimeGroupBox: TGroupBox;
    WaitingTimeTrackBar: TTrackBar;
    XMRWalletLabel: TLabel;
    SupportEmailLabel: TLabel;
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure ChkAppUpdatesButtonClick(Sender: TObject);
    procedure CleanLogButtonClick(Sender: TObject);
    procedure DefaultButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure HardResetButtonClick(Sender: TObject);
    procedure AppUpdateLabelClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LazSerial1RxData(Sender: TObject);
    procedure LazSerial1Status(Sender: TObject; Reason: THookSerialReason; const Value: string);
    procedure TimeLabelCLick(Sender: TObject);
    procedure ModesRadioGroupClick(Sender: TObject);
    procedure NetAddressEditClick(Sender: TObject);
    procedure NetAddressEditExit(Sender: TObject);
    procedure NetAddressEditKeyPress(Sender: TObject; var Key: char);
    procedure NetMonitoringCheckBoxChange(Sender: TObject);
    procedure PingTimeoutTrackBarChange(Sender: TObject);
    procedure PowerModeRadioGroupClick(Sender: TObject);
    procedure PowerOffButtonClick(Sender: TObject);
    procedure ReScanButtonClick(Sender: TObject);
    procedure SoftResetButtonClick(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure SupportEmailLabelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DeviceTimerTimer(Sender: TObject);
    procedure ChkUpdateTimerTimer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure TrayMenuItemExitClick(Sender: TObject);
    procedure TrayMenuItemRestoreClick(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: Integer; const Parameters: array of String);
    procedure WaitingTimeTrackBarChange(Sender: TObject);
    procedure WalletsLabelClick(Sender: TObject);
    procedure WalletsLabelMouseLeave(Sender: TObject);
    procedure WalletsLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TimeLabelMouseLeave(Sender: TObject);
    procedure TimeLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    const DsLabel = 'Device status: ';
    const DfwLabel = 'Device firmware version: ';
    const PingAddressBlank = 'paste here IP or URL address';
    const MinFWVers = $02;
    const ChkUpdateInterval = 6 * 3600000; //21600000 = 6 hours in ms.
    const clEnabled = $00AA00;

    var DeviceConnectionFlag: Boolean;
    var ChinaFlag: Boolean;
    var BasicFunctionFlag: Boolean;
    var BasicAcceptFlag: Boolean;
    var CheckCount: Integer;
    var FirmwareVersion: Integer;
    var NetHint: THintWindow;

    procedure SetRegistryAutorunKey(RunState: TRunState);
    function ExistRegistryAutorunKey(): Boolean;
    procedure ShowNetEditHint();
    function ValidateURL(const URL: String): Boolean;
    procedure ActivatePingInterface();
    procedure DeactivatePingInterface();
    procedure ActivateInterface();
    procedure DeactivateInterface();
    function SerialOpen(const Port: String): Boolean;
    procedure SerialClose();
    procedure SerialSendByte(CommandByte: Integer);
    procedure SerialSendCommand(Command: TSendCommand);
    function HexToVerString(IntVersion: Byte): String;
    procedure StartAppUpdate(const FileName: String);
    function GetAppUpdates(): TUpdateVersion;
    function CompareVersion(const UpdVers, AppVersion: String): Boolean;
    procedure InitDevConnection(const Message: String);
  public
    const AppName = 'USB WatchDog';
    const AppVersion = 'v0.1';
    const ManufacturerName = 'MB6718';
    const XMRWalletAddress = '42u7Gj1tUgRBo2V6SqcvyBdrF1mTN1rU62LcHFJdvYYr4vtwCxck5HdeMwfPWzLj7w2i6PsX2h64gfP5b84vWLceLdZyimg';
    const ETHWalletAddress = '0x044e4ba3369716158a67f1138cfc84984fb9fd2d';
    const BTCWalletAddress = '3CYsMhTT1qVvRXgJ6gc7kk3NiRnFxjCEJr';
    const SupportEmailAddress = 'support@usbwatchdog.ru';
    const MainURL = 'http://usbwatchdog.ru';
    const HelpURL = MainURL + '/help';
    const DownloadURL = MainURL + '/downloads/';
    const UpdateURL = MainURL + '/update';
    const AutorunKeyPath = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Run';

    { Config file }
    {$IFDEF UNIX} // -- UNIX --
      {$IFDEF LINUX}
        const ConfFile = 'usbwd.conf';
      {$ENDIF}
    {$ENDIF}
    {$IFDEF WINDOWS} // -- WINDOWS --
      const ConfFile = 'config.ini';
    {$ENDIF}
    const LogFile = 'Log.log';

    { Command list }
    const cmdHardReset = $FE;
    const cmdSoftReset = $FF;
    const cmdPowerOff = $FD;
    const cmdSoftMode = $A0;
    const cmdHardMode = $A1;
    const cmdPowerOffMode = $A2;
    const cmdAccept = $AA;
    const cmdHello = $80;
    const cmdCheckDevice = $81;
    const cmdGetDeviceVersion = $88;
  end;

var
  fMain: TfMain;

implementation

uses
  AppConfigs, Download, CustomMsgDlg;

{ TfMain }

procedure TfMain.SetRegistryAutorunKey(RunState: TRunState);
var
  R: TRegistry;
begin
  R:=TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
  with R do begin
    RootKey:=HKEY_CURRENT_USER;
    try
      OpenKey(AutorunKeyPath, False);
      case RunState of
        RunDisable: DeleteValue(AppName);
        RunEnable: WriteString(AppName,
          '"' + ExtractFilePath(ParamStr(0)) + ExtractFileName(ParamStr(0)) + '"');
      end;
    finally
      CloseKey;
      Free;
    end;
  end;
end;

function TfMain.ExistRegistryAutorunKey(): Boolean;
var
  R: TRegistry;
begin
  Result:=False;
  R:=TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
  with R do begin
    RootKey:=HKEY_CURRENT_USER;
    try
      OpenKeyReadOnly(AutorunKeyPath);
      if ValueExists(AppName) then
        Result:=True;
    finally
      CloseKey;
      Free;
    end;
  end;
end;

procedure TfMain.ShowNetEditHint();
var
  Rect: TRect;
begin
  NetAddressEdit.SetFocus;
  if NetHint <> nil then
    NetHint.ReleaseHandle;
  Rect:=NetHint.CalcHintRect(0, NetAddressEdit.Hint, nil);
  OffsetRect(
    Rect,
    fMain.Left + PingGroupBox.Left + NetAddressEdit.Left + 8,
    fMain.Top + PingGroupBox.Top + NetAddressEdit.Top + Rect.Height + 8
  );
  with NetHint do begin
    Color:=RGB(236, 226, 157);
    Font.Color:=clBlack;
    HideInterval:=5000;
    AutoHide:=True;
    ActivateHint(Rect, NetAddressEdit.Hint);
  end;
  NetMonitoringCheckBox.Checked:=False;
end;

function TfMain.ValidateURL(const URL: String): Boolean;
const
  RegExpString = '(^(([htps]{4,5}):\/{2})(([a-z0-9$_\.\+!\*\#39\(\),;\?&=-]|%[0-9a-f]' +
                 '{2})+(:([a-z0-9$_\.\+!\*\#39\(\),;\?&=-]|%[0-9a-f]{2})+)?@)?((([a-z' +
                 '0-9]\.|[a-z0-9][a-z0-9-]*[a-z0-9]\.)*[a-z][a-z0-9-]*[a-z0-9]|((\d|[' +
                 '1-9]\d|1\d{2}|2[0-4][0-9]|25[0-5])\.){3}(\d|[1-9]\d|1\d{2}|2[0-4][0' +
                 '-9]|25[0-5]))(:\d+)?)(((\/+([a-z0-9$_\.\+!\*\#39\(\),;:@&=-]|%[0-9a' +
                 '-f]{2})*)*(\?([a-z0-9$_\.\+!\*\#39\(\),;:@&=-]|%[0-9a-f]{2})*)?)?)?' +
                 '(#([a-z0-9$_\.\+!\*\#39\(\),;:@&=-]|%[0-9a-f]{2})*)?)$';
var
  RegExpr: TRegExpr;
  AddressString: String;
begin
  RegExpr:=TRegExpr.Create(RegExpString);
  try
    if Pos('.', URL) = 0 then
      AddressString:=''
    else
      if Pos('http://', URL) = 0 then
        AddressString:='http://' + URL
      else
        AddressString:=URL;
    if RegExpr.exec(AddressString) then
      Result:=True
    else
      Result:=False;
  finally
    RegExpr.Free();
  end;
end;

procedure TfMain.ActivatePingInterface();
begin
  NetAddressEdit.Enabled:=False;
  NetAddressEdit.ShowHint:=False;
  PingTimeoutLabel.Enabled:=True;
  PingTimeoutSecLabel.Enabled:=True;
  PingStatusLabel.Enabled:=True;
  PingStatusIndicatorLabel.Caption:='Enabled';
  PingStatusIndicatorLabel.Font.Color:=clEnabled;
  PingTimeoutTrackBar.Enabled:=True;
end;

procedure TfMain.DeactivatePingInterface();
begin
  NetAddressEdit.Enabled:=True;
  NetAddressEdit.ShowHint:=True;
  PingTimeoutLabel.Enabled:=False;
  PingStatusLabel.Enabled:=False;
  PingTimeoutSecLabel.Enabled:=False;
  PingStatusIndicatorLabel.Caption:='Disabled';
  PingStatusIndicatorLabel.Font.Color:=clGray;
  PingTimeoutTrackBar.Enabled:=False;
end;

procedure TfMain.ActivateInterface();
begin
  ImageList1.GetBitmap(2, StartStopButton.Glyph);
  PortSelectorComboBox.Enabled:=False;
  ReScanButton.Enabled:=False;
  IndicatorShape.Brush.Color:=RGBToColor(225, 203, 0);  // (88, 174, 0) darkgreen

  { PingGroupBox Disable }
  NetMonitoringCheckBox.Enabled:=False;
  NetAddressEdit.Enabled:=False;
  PingTimeoutTrackBar.Enabled:=False;
  PingTimeoutSecLabel.Enabled:=False;
  PingTimeoutLabel.Enabled:=False;
  PingStatusLabel.Enabled:=False;
end;

procedure TfMain.DeactivateInterface();
begin
  ImageList1.GetBitmap(0, StartStopButton.Glyph);
  PortSelectorComboBox.Enabled:=True;
  ReScanButton.Enabled:=True;
  IndicatorShape.Brush.Color:=RGBToColor(221, 0, 0);  // red

  ButtonsGroupBox.Enabled:=False;
  (ActionList1.ActionByName('SoftResetAction') as TAction).Enabled:=False;
  (ActionList1.ActionByName('HardResetAction') as TAction).Enabled:=False;
  WaitingTimeGroupBox.Enabled:=False;
  ModesRadioGroup.Enabled:=False;
  PowerModeRadioGroup.Enabled:=False;

  { PingGroupBox Enable }
  NetMonitoringCheckBox.Enabled:=True;
  if NetMonitoringCheckBox.Checked then begin
    NetAddressEdit.Enabled:=False;
    PingTimeoutTrackBar.Enabled:=True;
    PingTimeoutSecLabel.Enabled:=True;
    PingTimeoutLabel.Enabled:=True;
    PingStatusLabel.Enabled:=True;
  end
  else
    NetAddressEdit.Enabled:=True;
end;

function TfMain.SerialOpen(const Port: String): Boolean;
begin
  LazSerial1.Device:=Port;
  if not LazSerial1.Active then
    try
      LazSerial1.Open;
      Result:=True;
    except
      on E: Exception do begin
        ShowMessage(E.Message);
        Logger.Error(E.Message);
        Result:=False;
      end;
    end;
end;

procedure TfMain.SerialClose();
begin
  if LazSerial1.Active then
    try
      LazSerial1.Close;
    except
      on E: Exception do begin
        Logger.Error(E.Message);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TfMain.SerialSendByte(CommandByte: Integer);
begin
  if LazSerial1.Active then
    try
      LazSerial1.SynSer.SendByte(CommandByte);
    except
      on E: Exception do begin
        Logger.Error(E.Message);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TfMain.SerialSendCommand(Command: TSendCommand);
begin
  if LazSerial1.Active then
    case Command of
      SoftResetCmd: begin
        Logger.Info('Send "Soft Reset" action');
        SerialSendByte(cmdSoftReset);
      end;
      HardResetCmd: begin
        Logger.Info('Send "Hard Reset" action');
        SerialSendByte(cmdHardReset);
      end;
      PowerOffCmd: begin
        Logger.Info('Send "Power OFF" action');
        SerialSendByte(cmdPowerOff);
      end;
    end;
end;

function TfMain.HexToVerString(IntVersion: Byte): String;
begin
  if IntVersion = $00 then Result:='N/A' else
    Result:='v' + IntToStr(IntVersion div 10) + '.' + IntToStr(IntVersion mod 10);
end;

procedure TfMain.StartAppUpdate(const FileName: String);
const
  SetupFileName = 'setup.exe';
  CmdParam = '/verysilent';
begin
  try
    fDownload:=TfDownload.Create(Self);
    with fDownload do begin
      SetDownload(DownloadURL + FileName, SetupFileName);
      ShowModal;
    end;
    AppUpdateLabel.Visible:=False;
  finally
    fDownload.Free;
  end;
  ShellExecute(
    0,
    'open',
    PChar(SetupFileName),
    PChar(CmdParam),
    PChar(ExtractFilePath(SetupFileName)),
    1
  );
end;

function TfMain.GetAppUpdates(): TUpdateVersion;
const
  JSONPath = 'versions.win.last_stable.';
var
  FHTTPClient: TFPHTTPClient;
  JsonString: String;
  JSON: TJSONData;
begin
  FHTTPClient:=TFPHTTPClient.Create(nil);
  with Result do
    try
      try
        JsonString:=FHTTPClient.Get(UpdateURL);
        Logger.Info('Server connection established.');
        if (JsonString <> '') then
          try
            JSON:=GetJSON(JsonString);
            with JSON do begin
              prefix:=FindPath(JSONPath + 'prefix').AsString;
              version:=FindPath(JSONPath + 'version').AsString;
              build:=FindPath(JSONPath + 'build').AsString;
              date:=FindPath(JSONPath + 'date').AsString;
              filename:=FindPath(JSONPath + 'filename').AsString;
            end;
          finally
            JSON.Free;
          end
        else
          raise Exception.Create('Server response is empty!');
      except
        on E: Exception do begin
          Logger.Error(E.Message);
          MessageDlg('App update error!', E.Message, mtError, [mbOK], 0);
        end;
      end;
    finally
      FHTTPClient.Free;
    end;
end;

function TfMain.CompareVersion(const UpdVers, AppVersion: String): Boolean;
var
  fs: TFormatSettings;
begin
  if (UpdVers <> '') then begin
    fs.DecimalSeparator:='.';
    if StrToFloat(UpdVers, fs) > StrToFloat(AppVersion, fs) then begin
      Result:=True;
      Exit();
    end;
  end;
  Result:=False;
end;

procedure TfMain.InitDevConnection(const Message: String);
begin
  DeviceStatusLabel.Caption:=DsLabel + Message;
  FirmwareVersionLabel.Caption:=DfwLabel + 'undefined';
  FirmwareVersion:=0;
  CheckCount:=0;
  ChinaFlag:=True;
  DeviceConnectionFlag:=False;
  BasicFunctionFlag:=False;
  BasicAcceptFlag:=False;
end;

procedure TfMain.Timer1Timer(Sender: TObject);
begin
  CopiedLabel.Visible:=False;
  Timer1.Enabled:=False;
end;

procedure TfMain.DeviceTimerTimer(Sender: TObject);
var
  PingSend: TPINGSend;
  PingTime: Integer;
begin
  if NetMonitoringCheckBox.Checked and DeviceConnectionFlag then begin
    PingSend:=TPINGSend.Create;
    try
      PingSend.Timeout:=PingTimeoutTrackBar.Position;
      if PingSend.Ping(NetAddressEdit.Text) then begin
        PingStatusIndicatorLabel.Font.Color:=clEnabled;
        PingTime:=PingSend.PingTime;
        if PingTime < PingSend.Timeout then begin
          PingStatusIndicatorLabel.Caption:='Reply from in: ' + IntToStr(PingTime) + ' ms';
          Logger.Info('Ping on ' + NetAddressEdit.Text + ' - ' + PingStatusIndicatorLabel.Caption);
        end
        else begin
          PingStatusIndicatorLabel.Font.Color:=clRed;
          PingStatusIndicatorLabel.Caption:='No response in: ' + IntToStr(PingSend.Timeout) + ' ms';
          Logger.Info('Ping on ' + NetAddressEdit.Text + ' - ' + PingStatusIndicatorLabel.Caption);
          SerialSendCommand(TSendCommand(ModesRadioGroup.ItemIndex));
        end;
      end;
    finally
      PingSend.Free;
    end;
  end;

  if DeviceConnectionFlag then
    SerialSendByte(cmdHello)
  else begin
    SerialClose();
    DeactivateInterface();
    DeviceTimer.Enabled:=False;
  end;
  DeviceConnectionFlag:=False;
end;

procedure TfMain.ChkUpdateTimerTimer(Sender: TObject);
var
  UpdateVersion: TUpdateVersion;
  CustomMsgDlg: TCustomMsgDlg;
begin
  ChkUpdateTimer.Enabled:=False;
  UpdateVersion:=GetAppUpdates();
  with UpdateVersion do
    if CompareVersion(version, Copy(AppVersion, 2, 4)) then begin
      AppUpdateLabel.Visible:=True;
      AppUpdateLabel.Caption:='  App update available, version: v' + version + '  ';
      CustomMsgDlg:=TCustomMsgDlg.Create(
        'App Update. ',
        'Update available. Update app now?',
        mtConfirmation,
        [
          CustomMsgDlgButton(mbNo, 'Ask later', TimeMark),
          CustomMsgDlgButton(mbYes, 'Update now')
        ],
        Self,
        mrNo,
        10
      );
      try
        if CustomMsgDlg.ShowDialog = mrYes then
          StartAppUpdate(filename);
      finally
        CustomMsgDlg.Free;
      end;
    end;
  ChkUpdateTimer.Interval:=ChkUpdateInterval;
  ChkUpdateTimer.Enabled:=True;
end;

procedure TfMain.TrayIcon1DblClick(Sender: TObject);
begin
  TrayIcon1.Visible:=False;
  WindowState:=wsNormal;
  Show;
end;

procedure TfMain.TrayMenuItemExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfMain.TrayMenuItemRestoreClick(Sender: TObject);
begin
  TrayIcon1DblClick(Self);
end;

procedure TfMain.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer;
  const Parameters: array of String);
begin
  BringToFront;
  if TrayIcon1.Visible then begin
    TrayIcon1.Visible:=False;
    WindowState:=wsNormal;
    Show;
  end else begin
    FormStyle:=fsSystemStayOnTop;
    FormStyle:=fsNormal;
    WindowState:=wsNormal;
  end;
end;

procedure TfMain.WaitingTimeTrackBarChange(Sender: TObject);
begin
  WaitingSecLabel.Caption:=IntToStr(WaitingTimeTrackBar.Position * 10) + ' sec';
  if LazSerial1.Active then
    Logger.Info('Timeout changet on ' + WaitingSecLabel.Caption);
end;

procedure TfMain.CheckBox7Change(Sender: TObject);
begin
  if CheckBox7.Checked then
    CheckBox8.Enabled:=True
  else
    CheckBox8.Enabled:=False;
end;

procedure TfMain.ChkAppUpdatesButtonClick(Sender: TObject);
var
  UpdateVersion: TUpdateVersion;
begin
  Logger.Info('"Check Update" button pushed.');
  UpdateVersion:=GetAppUpdates();
  with UpdateVersion do
    if version <> '' then
      if CompareVersion(version, Copy(AppVersion, 2, 4)) then begin
        Logger.Info('Update is available. Current app version: ' + AppVersion +
          ' New version: ' + version);
        case MessageDlg(
          'App update',
          'A new version of the application is available.' + LineEnding + LineEnding +
          'Prefix: ' + prefix + LineEnding +
          'Version: ' + version + LineEnding +
          'Build: ' + build + LineEnding +
          'Release Date: ' + date + LineEnding + LineEnding +
          'Update now?',
          mtConfirmation,
          mbYesNo,
          0
        ) of
          mrYes: StartAppUpdate(filename);
          mrNo, mrCancel: Logger.Info('Update canceled.');
        end;
      end
      else begin
        Logger.Info('No update required.');
        MessageDlg(
          'App update',
          'No update required.' + LineEnding +
          'The current application version is the latest stable version.',
          mtInformation, [mbOK], 0);
      end;
end;

procedure TfMain.CheckBox4Change(Sender: TObject);
begin
  if CheckBox4.Checked then
    Logger.SetNoisyMode
  else begin
    Logger.Info('Disabled logging');
    Logger.SetQuietMode;
  end;
end;

procedure TfMain.CheckBox2Change(Sender: TObject);
begin
  if CheckBox2.Checked then
    SetRegistryAutorunKey(RunEnable)
  else
    SetRegistryAutorunKey(RunDisable);
end;

procedure TfMain.CheckBox3Change(Sender: TObject);
begin
  if CheckBox3.Checked then begin
    ChkUpdateTimer.Interval:=5000;
    ChkUpdateTimer.Enabled:=True;
  end
  else
    ChkUpdateTimer.Enabled:=False;
end;

procedure TfMain.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    Logger.Info('Enabled logging');
end;

procedure TfMain.CleanLogButtonClick(Sender: TObject);
begin
  if MessageDlg(
    'Clean log file',
    'Are you sure a want to clear log file?',
    mtWarning,
    mbYesNo,
    0
  ) = mrYes then
    Logger.Clear;
end;

procedure TfMain.DefaultButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if MessageDlg(
    'Default app settings',
    'Are you sure you want to set the default app settings?',
    mtInformation,
    mbYesNo,
    0
  ) = mrYes then begin
    for i:=0 to ComponentCount - 1 do
      if (Components[i] is TCheckBox) and (PageControl1.ActivePage = AppTabSheet) then
        (Components[i] as TCheckBox).Checked:=False;
    WaitingTimeTrackBar.Position:=180;
    PowerModeRadioGroup.ItemIndex:=0;
    ModesRadioGroup.ItemIndex:=1;
    PingTimeoutTrackBar.Position:=1000;
    NetAddressEdit.Clear;
    NetAddressEditExit(Self);
    NetMonitoringCheckBox.Checked:=False;
  end;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CheckBox5.Checked and CheckBox6.Checked then begin
    CloseAction:=caNone;
    TrayIcon1.Visible:=True;
    Hide;
  end
  else
    if CheckBox5.Checked and (WindowState = wsNormal) then begin
      CloseAction:=caNone;
      PostMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    end;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Logger:=TLogger.Create(LogFile);
  NetHint:=HintWindowClass.Create(Self);

  fMain.Caption:=AppName + ' ' + AppVersion;
  Application.Title:=AppName + ' ' + AppVersion;
  SupportEmailLabel.Caption:=SupportEmailAddress;
  BTCWalletLabel.Caption:=BTCWalletAddress;
  ETHWalletLabel.Caption:=ETHWalletAddress;
  XMRWalletLabel.Caption:=Copy(XMRWalletAddress, 0, 32) + ' ... ' +
    Copy(XMRWalletAddress, Length(XMRWalletAddress) - 32, Length(XMRWalletAddress));

  TrayIcon1.Icon:=Application.Icon;
  TrayIcon1.Hint:=AppName + AppVersion;

  PageControl1.ActivePageIndex:=0;

  { App Load Config section }
  ReadAppConfigs(ConfFile);
  WaitingTimeTrackBar.Position:=ResetTimeout div 10;
  if USBPwrMode > 0 then begin
    PowerModeRadioGroup.ItemIndex:=USBPwrMode;
    PowerOffButton.Enabled:=True;
    ModesRadioGroup.Controls[2].Enabled:=True;
  end
  else begin
    PowerOffButton.Enabled:=False;
    ModesRadioGroup.Controls[2].Enabled:=False;
  end;
  ModesRadioGroup.ItemIndex:=ResetMode;
  if AutoConnect then
    CheckBox8.Checked:=True;
  if UseLog then begin
    Logger.Head('Logging initialized');
    Logger.Info('Start application');
    CheckBox4.Checked:=True;
  end
  else
    Logger.SetQuietMode;
  if MinimizeOnClose then
    CheckBox5.Checked:=True;
  if inSysTray then
    CheckBox6.Checked:=True;
  if LowerCase(WinStartState) = 'minimized' then begin
    CheckBox1.Checked:=True;
    PostMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  end;
  if ExistRegistryAutorunKey() then begin
    CheckBox2.Checked:=True;
    if not LaunchWithOS then
      LaunchWithOS:=True;
  end
  else
    if LaunchWithOS then
      CheckBox2.Checked:=True
    else
      CheckBox2.Checked:=False;
  if NetAddress <> '' then
    NetAddressEdit.Text:=NetAddress;
  if PingTimeOut > 0 then
    PingTimeoutTrackBar.Position:=PingTimeOut;
  NetMonitoringCheckBox.Checked:=NetMonitoring;
  if AutoChkUpdates then
    CheckBox3.Checked:=True;

  PortSelectorComboBox.Items.CommaText:=GetSerialPortNames();
  Logger.Info('Finded ports: ' + IntToStr(PortSelectorComboBox.Items.Count));
  Logger.Info('Ports: ' + PortSelectorComboBox.Items.Text.Replace(LineEnding, ', '));
  if PortSelectorComboBox.Items.Count > 0 then begin
    PortSelectorComboBox.ItemIndex:=0;
    if DefaultPort <> '' then begin
      CheckBox7.Checked:=True;
      for i:=0 to PortSelectorComboBox.Items.Count - 1 do
        if DefaultPort = PortSelectorComboBox.Items[i] then begin
          PortSelectorComboBox.ItemIndex:=i;
          Logger.Info('Selected default port: ' + DefaultPort);
          if CheckBox8.Checked then
            StartStopButtonClick(Self);
          Break;
        end;
        if (i = PortSelectorComboBox.Items.Count - 1) and
           (DefaultPort <> PortSelectorComboBox.Items[i]) then begin
          MessageDlg(
            'Error COM port',
            'Port "' + DefaultPort + '" not exist, please select other port',
            mtError, [mbOK], 0);
          PortSelectorComboBox.Text:=DefaultPort;
          Logger.Error('Port "' + DefaultPort + '" not exist');
        end;
    end;
    StartStopButton.Enabled:=True;
    IndicatorShape.Brush.Color:=RGBToColor(221, 0, 0);  // red
  end;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  if LazSerial1.Active then begin
    SerialClose();
    DeviceTimer.Enabled:=False;
    DeactivateInterface();
  end;

  { App Save Config Section}
  ResetTimeout:=WaitingTimeTrackBar.Position * 10;
  USBPwrMode:=PowerModeRadioGroup.ItemIndex;
  ResetMode:=ModesRadioGroup.ItemIndex;
  WinPosX:=fMain.Left;
  WinPosY:=fMain.Top;
  if CheckBox7.Checked then
    DefaultPort:=PortSelectorComboBox.Text
  else
    DefaultPort:='';
  AutoConnect:=CheckBox8.Checked;
  UseLog:=CheckBox4.Checked;
  MinimizeOnClose:=CheckBox5.Checked;
  inSysTray:=CheckBox6.Checked;
  if CheckBox1.Checked then
    WinStartState:='Minimized'
  else
    WinStartState:='Normal';
  LaunchWithOS:=CheckBox2.Checked;
  NetMonitoring:=NetMonitoringCheckBox.Checked;
  if NetMonitoring and (NetAddressEdit.Text <> '') and (NetAddressEdit.Text <> PingAddressBlank) then
    NetAddress:=NetAddressEdit.Text
  else
    NetAddress:='';
  PingTimeOut:=PingTimeoutTrackBar.Position;
  AutoChkUpdates:=CheckBox3.Checked;
  WriteAppConfigs(ConfFile);

  Logger.Info('Close application');
  Logger.SetQuietMode;

  NetHint.Free;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  WindowState:=wsNormal;
  if (WinPosX <> -1) and (WinPosY <> -1) then begin
     fMain.Left:=WinPosX;
     fMain.Top:=WinPosY;
  end;
end;

procedure TfMain.FormWindowStateChange(Sender: TObject);
begin
  if CheckBox6.Checked and (WindowState = wsMinimized) then begin
    TrayIcon1.Visible:=True;
    TrayIcon1.ShowBalloonHint;
    Hide;
  end;
  WinPosX:=fMain.Left;
  WinPosY:=fMain.Top;
end;

procedure TfMain.LazSerial1RxData(Sender: TObject);
var
  Response: Integer;
  CustomMsgDlg: TCustomMsgDlg;
begin { TODO : Очищать буфер во время ожидания RemainMsgDlg }
  Response:=LazSerial1.SynSer.RecvByte(0);
  case Response of
    $81: begin
      DeviceConnectionFlag:=True;

      Logger.Info('Send signal OK');
      IndicatorShape.Brush.Color:=RGBToColor(102, 204, 0); // green
      ButtonsGroupBox.Enabled:=True;

      (ActionList1.ActionByName('SoftResetAction') as TAction).Enabled:=True;
      (ActionList1.ActionByName('HardResetAction') as TAction).Enabled:=True;

      WaitingTimeGroupBox.Enabled:=True;
      DeviceStatusLabel.Caption:=DsLabel + 'connected';

      if CheckCount > 1 then begin
        FirmwareVersionLabel.Caption:=DfwLabel + 'CHINA';
        Logger.Info('Identified device as CHINA firmware');
      end
      else begin
        SerialSendByte(cmdCheckDevice);
        if ChinaFlag then
          Inc(CheckCount);
      end;

      SerialSendByte(WaitingTimeTrackBar.Position);
      Logger.Info('Send Timeout on ' + WaitingSecLabel.Caption);
    end;
    $80: begin
      ChinaFlag:=False;

      if FirmwareVersion > 0 then begin
        Logger.Info('Identified device as firmware MB6718 ' + HexToVerString(FirmwareVersion));
        FirmwareVersionLabel.Caption:=DfwLabel + ManufacturerName + ' ' + HexToVerString(FirmwareVersion);
        if FirmwareVersion < MinFWVers then begin
          BasicFunctionFlag:=False;
          if not BasicAcceptFlag then begin
            Logger.Warning('Outdated firmware version! Device is put into a basic operation mode');
            DeviceTimer.Enabled:=False;
            CustomMsgDlg:=TCustomMsgDlg.Create(
              'App warning! ',
              'Current device version is ' + HexToVerString(FirmwareVersion) + LineEnding +
              'The application requires a device version ' + HexToVerString(MinFWVers) +
              ' or higher.' + LineEnding + LineEnding +
              'Will only basic functions be used (automatically)' + LineEnding +
              'or check app update now (recommended)?',
              mtWarning,
              [
                CustomMsgDlgButton(mbYes, '&Yes'),
                CustomMsgDlgButton(mbIgnore, 'Basic', TimeMark)
              ],
              Self,
              mrIgnore,
              5
            );
            try
              case CustomMsgDlg.ShowDialog of
                mrOK, mrYes: AppUpdateLabelClick(Self);
                mrIgnore: { nop } ; //ShowMessage('Pushed by: Basic');
                mrCancel: { nop } ; //ShowMessage('Pushed by: Cancel')
              end;
            finally
              CustomMsgDlg.Free;
            end;
            BasicAcceptFlag:=True;
            DeviceTimer.Enabled:=True;
          end;
        end
        else begin
          Logger.Info('Device in extended mode');
          BasicFunctionFlag:=True;
        end;
      end
      else begin
        FirmwareVersionLabel.Caption:=DfwLabel + ManufacturerName;
        SerialSendByte(cmdGetDeviceVersion);
      end;

      if BasicFunctionFlag then begin
        ModesRadioGroup.Enabled:=True;
        PowerModeRadioGroup.Enabled:=True;
        PowerModeRadioGroupClick(Self);
        Sleep(200);  // Hard delay
        ModesRadioGroup.OnClick(Self);
      end;
    end;
    $01..$7F: begin
      if CheckCount < 2 then
        FirmwareVersion:=Response;
    end;
  end;
end;

procedure TfMain.LazSerial1Status(Sender: TObject; Reason: THookSerialReason; const Value: string);
begin
  case Reason of
    HR_SerialClose: begin
      InitDevConnection('Not connected');
      Logger.Info('Port disconnected');
    end;
    HR_Connect: begin
      InitDevConnection('Try to connect');
      Logger.Info('Port connected');
    end;
  end;
end;

procedure TfMain.PowerModeRadioGroupClick(Sender: TObject);
begin
  if PowerModeRadioGroup.ItemIndex=1 then begin
    PowerOffButton.Enabled:=True;
    (ActionList1.ActionByName('PowerOffAction') as TAction).Enabled:=True;
    ModesRadioGroup.Controls[2].Enabled:=True;
  end
  else begin
    PowerOffButton.Enabled:=False;
    (ActionList1.ActionByName('PowerOffAction') as TAction).Enabled:=False;
    ModesRadioGroup.Controls[2].Enabled:=False;
  end;
end;

procedure TfMain.ReScanButtonClick(Sender: TObject);
begin
  PortSelectorComboBox.Items.CommaText:=GetSerialPortNames();
  if PortSelectorComboBox.Items.Count > 0 then
    PortSelectorComboBox.ItemIndex:=0;
end;

procedure TfMain.SoftResetButtonClick(Sender: TObject);
begin
  SerialSendCommand(SoftResetCmd);
end;

procedure TfMain.HardResetButtonClick(Sender: TObject);
begin
  SerialSendCommand(HardResetCmd);
end;

procedure TfMain.AppUpdateLabelClick(Sender: TObject);
begin
  ChkAppUpdatesButtonClick(Self);
end;

procedure TfMain.HelpButtonClick(Sender: TObject);
begin
  OpenURL(HelpURL);
end;

procedure TfMain.PowerOffButtonClick(Sender: TObject);
begin
  SerialSendCommand(PowerOffCmd);
end;

procedure TfMain.StartStopButtonClick(Sender: TObject);
begin
  if LazSerial1.Active then begin
    Logger.Info('Close ' + PortSelectorComboBox.Text + ' port and session');
    SerialClose();
    DeviceTimer.Enabled:=False;
    DeactivateInterface();
  end
  else begin
    Logger.Info('Trying to open ' + PortSelectorComboBox.Text + ' port and session');
    if SerialOpen(PortSelectorComboBox.Text) then begin
      ActivateInterface();
      SerialSendByte(cmdHello);
      DeviceTimer.Enabled:=True;
    end;
  end;
end;

procedure TfMain.SupportEmailLabelClick(Sender: TObject);
begin
  OpenURL('mailto:' + SupportEmailAddress);
end;

procedure TfMain.WalletsLabelClick(Sender: TObject);
begin
  if (Sender is TLabel) then
    case (Sender as TLabel).Tag of
      1: Clipboard.AsText:=XMRWalletAddress;
      2: Clipboard.AsText:=ETHWalletAddress;
      3: Clipboard.AsText:=BTCWalletAddress;
    end;
  CopiedLabel.Visible:=True;
  Timer1.Enabled:=True;
end;

procedure TfMain.ModesRadioGroupClick(Sender: TObject);
begin
  if LazSerial1.Active then
    case ModesRadioGroup.ItemIndex of
      0: begin
        Logger.Info('Send "Change on Soft Mode" action');
        SerialSendByte(cmdSoftMode);
      end;
      1: begin
        Logger.Info('Send "Change on Hard Mode" action');
        SerialSendByte(cmdHardMode);
      end;
      2: begin
        Logger.Info('Send "Change on Power OFF Mode" action');
        SerialSendByte(cmdPowerOffMode);
      end;
    end;
end;

procedure TfMain.NetAddressEditClick(Sender: TObject);
begin
  with NetAddressEdit do
    if Text = PingAddressBlank then
      Text:='';
end;

procedure TfMain.NetAddressEditExit(Sender: TObject);
begin
  if NetHint <> nil then
    NetHint.ReleaseHandle;
  with NetAddressEdit do
    if Text = '' then begin
      Text:=PingAddressBlank;
      Font.Color:=clSilver;
    end;
end;

procedure TfMain.NetAddressEditKeyPress(Sender: TObject; var Key: char);
begin
  NetAddressEdit.Font.Color:=clDefault;
end;

procedure TfMain.NetMonitoringCheckBoxChange(Sender: TObject);
begin
  if NetMonitoringCheckBox.Checked then
    if (NetAddressEdit.Text = PingAddressBlank) then begin
      MessageDlg(
        'Network address warning.',
        'No network address entered! Please enter an address.',
        mtWarning, [mbOK], 0);
      ShowNetEditHint();
    end
    else
      {if IsIP(NetAddressEdit.Text) or IsIP6(NetAddressEdit.Text) then
        ActivatePingInterface()}
      if ValidateURL(NetAddressEdit.Text) then
        ActivatePingInterface()
      else
        ShowNetEditHint()
  else
    DeactivatePingInterface();
end;

procedure TfMain.PingTimeoutTrackBarChange(Sender: TObject);
begin
  PingTimeoutSecLabel.Caption:=IntToStr(PingTimeoutTrackBar.Position) + ' ms';
end;

procedure TfMain.TimeLabelCLick(Sender: TObject);
var
  CaptionString: String;
begin
  if (Sender is TLabel) then begin
    CaptionString:=(Sender as TLabel).Caption;
    case Copy(CaptionString, Length(CaptionString), 1) of
      's': WaitingTimeTrackBar.Position:=StrToInt(
             Copy(CaptionString, 0, Length(CaptionString) - 1)) div 10;
      'm': WaitingTimeTrackBar.Position:=(StrToInt(
             Copy(CaptionString, 0, Length(CaptionString) - 1)) * 60) div 10;
    end;
  end;
end;

procedure TfMain.WalletsLabelMouseLeave(Sender: TObject);
begin
  if (Sender is TLabel) then
    (Sender as TLabel).Font.Color:=clHighlight;
end;

procedure TfMain.WalletsLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TLabel) then
    (Sender as TLabel).Font.Color:=$8000FF;
end;

procedure TfMain.TimeLabelMouseLeave(Sender: TObject);
begin
  if (Sender is TLabel) then
    (Sender as TLabel).Font.Style:=(Sender as TLabel).Font.Style-[fsBold];
end;

procedure TfMain.TimeLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TLabel) then
    (Sender as TLabel).Font.Style:=(Sender as TLabel).Font.Style+[fsBold];
end;

initialization
  {$R *.lfm}

end.

