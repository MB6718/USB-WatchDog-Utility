unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, Clipbrd, Menus, LazSerial, LazSynaSer,
  Log4Pascal, windows, registry, RegExpr, pingsend, LCLIntf, LCLType;

type

  TRunState = (RunDisable, RunEnable);
  TSendCommand = (SoftResetCmd, HardResetCmd, PowerOffCmd);

  { TfMain }

  TfMain = class(TForm)
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
    Shape2: TShape;
    SoftResetButton: TButton;
    StartStopButton: TBitBtn;
    Timer1: TTimer;
    Timer2: TTimer;
    TitleLabel: TLabel;
    TrayIcon1: TTrayIcon;
    TrayMenuItemExit: TMenuItem;
    TrayMenuItemRestore: TMenuItem;
    TrayMenuSep1: TMenuItem;
    TrayPopupMenu: TPopupMenu;
    WaitingSecLabel: TLabel;
    WaitingTimeGroupBox: TGroupBox;
    WaitingTimeTrackBar: TTrackBar;
    XMRWalletLabel: TLabel;
    SupportEmailLabel: TLabel;
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CleanLogButtonClick(Sender: TObject);
    procedure DefaultButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure HardResetButtonClick(Sender: TObject);
    procedure LazSerial1RxData(Sender: TObject);
    procedure LazSerial1Status(Sender: TObject; Reason: THookSerialReason;
      const Value: string);
    procedure m10LabelClick(Sender: TObject);
    procedure m15LabelClick(Sender: TObject);
    procedure m20LabelClick(Sender: TObject);
    procedure m3LabelClick(Sender: TObject);
    procedure m5LabelClick(Sender: TObject);
    procedure ModesRadioGroupClick(Sender: TObject);
    procedure NetAddressEditClick(Sender: TObject);
    procedure NetAddressEditExit(Sender: TObject);
    procedure NetAddressEditKeyPress(Sender: TObject; var Key: char);
    procedure NetMonitoringCheckBoxChange(Sender: TObject);
    procedure PingTimeoutTrackBarChange(Sender: TObject);
    procedure PowerModeRadioGroupClick(Sender: TObject);
    procedure PowerOffButtonClick(Sender: TObject);
    procedure ReScanButtonClick(Sender: TObject);
    procedure s10LabelClick(Sender: TObject);
    procedure SoftResetButtonClick(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure SupportEmailLabelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure TrayMenuItemExitClick(Sender: TObject);
    procedure TrayMenuItemRestoreClick(Sender: TObject);
    procedure WaitingTimeTrackBarChange(Sender: TObject);
    procedure WalletsLabelClick(Sender: TObject);
    procedure WalletsLabelMouseLeave(Sender: TObject);
    procedure WalletsLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TimeLabelMouseLeave(Sender: TObject);
    procedure TimeLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure ActivateInterface();
    procedure DeactivateInterface();
    function SerialOpen(Port: String): Boolean;
    procedure SerialClose();
    procedure SerialSendByte(CommandByte: Integer);
    procedure RegRunKey(RunState: TRunState);
    procedure NetEditShowHint();
    function ValidURL(URL: String): Boolean;
    procedure ActivatePingInterface();
    procedure DeactivatePingInterface();
    procedure SerialSendCommand(Command: TSendCommand);
    function HexToVerStr(IntVersion: Byte): String;
    function RemainMsgDlg(const aMsgCaption, aMsgText: String; aDlgType: TMsgDlgType;
      aTimeOut: Integer): TModalResult;
    procedure MsgDlgOnTimer(Sender: TObject);
  public
    const AppName = 'USB WatchDog';
    const AppVers = 'v0.1';
    const Manufacturer = 'MB6718';
    const ds_label = 'Device status: ';
    const dfw_label = 'Device firmware version: ';
    const xmr_wallet = 'xmr_address';
    const eth_wallet = 'eth_address';
    const btc_wallet = 'btc_address';
    const PingAddressBlank = 'paste here IP or URL address';
    const MinFWVers = $02;

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
    const hard_reset_cmd = $FE;
    const soft_reset_cmd = $FF;
    const power_off_cmd = $FD;
    const soft_mode_cmd = $A0;
    const hard_mode_cmd = $A1;
    const power_off_mode_cmd = $A2;
    const accept_cmd = $AA;
    const hello_cmd = $80;
    const check_device_cmd = $81;
    const get_device_version_cmd = $88;

    const clEnabled = $00AA00;

    var device_conn_flag: Boolean;
    var china_flag: Boolean;
    var basic_func_flag: Boolean;
    var basic_accept_flag: Boolean;
    var check_count: Integer;
    var fw_version: Integer;
    var NetHint: THintWindow;
    var ARemainMsgDlg: TForm;
    var aRemainMsgDlgCaption,
        aRemainBtnCaption: String;
    var aRemainTime: Integer;
  end;

var
  fMain: TfMain;

implementation

uses
  AppConfigs;

{ TfMain }

procedure TfMain.RegRunKey(RunState: TRunState);
const
  keypath = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
var
  Reg: TRegistry;
begin
  Reg:=TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
  with Reg do begin
    try
      RootKey:=HKEY_CURRENT_USER;
      OpenKey(keypath, false);
      case RunState of
        RunDisable: DeleteValue(AppName);
        RunEnable: WriteString(AppName, '"' + ParamStr(0) + '"');
      end;
    finally
      CloseKey;
      Free;
    end;
  end;
end;

procedure TfMain.NetEditShowHint();
var
  rect: TRect;
begin
  NetAddressEdit.SetFocus;
  if NetHint <> nil then
    NetHint.ReleaseHandle;
  rect:=NetHint.CalcHintRect(0, NetAddressEdit.Hint, nil);
  OffsetRect(
    rect,
    fMain.Left + PingGroupBox.Left + NetAddressEdit.Left + 8,
    fMain.Top + PingGroupBox.Top + NetAddressEdit.Top + rect.Height + 8
    //fMain.Top + PingGroupBox.Top + NetAddressEdit.Top + rect.Height + NetAddressEdit.Height
  );
  with NetHint do begin
    Color:=RGB(236, 226, 157);
    Font.Color:=clBlack;
    HideInterval:=5000;
    AutoHide:=True;
    ActivateHint(rect, NetAddressEdit.Hint);
  end;
  NetMonitoringCheckBox.Checked:=False;
end;

function TfMain.ValidURL(URL: String): Boolean;
const
  regexp_string = '(^(([htps]{4,5}):\/{2})(([a-z0-9$_\.\+!\*\#39\(\),;\?&=-]|%[0-9a-f]{2})+(:([a-z0-9$_\.\+!\*\#39\(\),;\?&=-]|%[0-9a-f]{2})+)?@)?((([a-z0-9]\.|[a-z0-9][a-z0-9-]*[a-z0-9]\.)*[a-z][a-z0-9-]*[a-z0-9]|((\d|[1-9]\d|1\d{2}|2[0-4][0-9]|25[0-5])\.){3}(\d|[1-9]\d|1\d{2}|2[0-4][0-9]|25[0-5]))(:\d+)?)(((\/+([a-z0-9$_\.\+!\*\#39\(\),;:@&=-]|%[0-9a-f]{2})*)*(\?([a-z0-9$_\.\+!\*\#39\(\),;:@&=-]|%[0-9a-f]{2})*)?)?)?(#([a-z0-9$_\.\+!\*\#39\(\),;:@&=-]|%[0-9a-f]{2})*)?)$';
var
  regexpr: TRegExpr;
  address_string: String;
begin
  regexpr:=TRegExpr.Create(regexp_string);
  try
    if pos('.', URL) = 0 then
      address_string:=''
    else
      if pos('http://', URL) = 0 then
        address_string:='http://' + URL
      else
        address_string:=URL;
    if regexpr.exec(address_string) then
      Result:=True
    else
      Result:=False;
  finally
    regexpr.Free();
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

procedure TfMain.SerialSendCommand(Command: TSendCommand);
begin
  if LazSerial1.Active then
    case Command of
      SoftResetCmd: begin
        Logger.Info('Call "Soft Reset" action');
        SerialSendByte(soft_reset_cmd);
      end;
      HardResetCmd: begin
        Logger.Info('Call "Hard Reset" action');
        SerialSendByte(hard_reset_cmd);
      end;
      PowerOffCmd: begin
        Logger.Info('Call "Power OFF" action');
        SerialSendByte(power_off_cmd);
      end;
    end;
end;

function TfMain.HexToVerStr(IntVersion: Byte): String;
begin
  if IntVersion = $00 then Result:='N/A' else
    Result:='v' + IntToStr(IntVersion div 10) + '.' + IntToStr(IntVersion mod 10);
end;

function TfMain.RemainMsgDlg(const aMsgCaption, aMsgText: String; aDlgType: TMsgDlgType;
  aTimeOut: Integer): TModalResult;

  function FindControlByCaption(const p: TForm; const aCaption: string): TControl;
  var
    i: LongInt;
    c: TControl;
  begin
    Result:=nil;
    if (aCaption = '') or (p = nil) then exit;
    for i:=0 to p.ControlCount - 1 do begin
      if p.Controls[i] is TBitBtn then begin
        c:=TBitBtn(p.Controls[i]);
        if (CompareText(c.Caption, aCaption) = 0) then
          Exit(c);
      end;
    end;
  end;

const
  BtnMargin = 10;
var
  Timer: TTimer;
  Panel: TPanel;
  YesButton, CancelButton: TButton;
  Control: TControl;
begin
  ARemainMsgDlg:=CreateMessageDialog(aMsgText, aDlgType, [mbYes]);
  Panel:=TPanel.Create(ARemainMsgDlg);
  YesButton:=TButton.Create(Panel);
  CancelButton:=TButton.Create(Panel);
  Timer:=TTimer.Create(ARemainMsgDlg);
  Timer.Interval:=1000;
  Timer.Enabled:=False;
  Timer.OnTimer:=@MsgDlgOnTimer;
  aRemainTime:=aTimeOut;
  aRemainMsgDlgCaption:=aMsgCaption + 'Remain %d sec.';
  aRemainBtnCaption:='Basic (%d)';
  with ARemainMsgDlg do begin
    try
      Caption:=aRemainMsgDlgCaption;
      Width:=400;
      ParentColor:=False;
      Color:=clWindow;
      //Position:=poOwnerFormCenter;
      Control:=FindControlByCaption(ARemainMsgDlg, '&Yes');
      if ((Control <> nil) and (Control is TBitBtn)) then
        (Control as TBitBtn).Visible:=False;
      with Panel do begin
        Parent:=ARemainMsgDlg;
        ParentColor:=False;
        Color:=clDefault;
        Caption:=' ';
        Align:=alBottom;
        Height:=45;
        BevelInner:=bvLowered;
        BevelOuter:=bvNone;
        Name:='Panel';
      end;
      with YesButton do begin
        Parent:=Panel;
        Width:=100;
        Left:=CancelButton.Left + CancelButton.Width + Width + BtnMargin;
        Top:=(Panel.Height div 2) + (Height div 2) - Height + 1;
        Caption:=aRemainBtnCaption;
        ModalResult:=mrIgnore;
        Font.Style:=[fsBold];
        Name:='YesButton';
      end;
      with CancelButton do begin
        Parent:=Panel;
        Width:=100;
        Left:=ARemainMsgDlg.Width - Width - BtnMargin;
        Top:=(Panel.Height div 2) + (Height div 2) - Height + 1;
        Caption:='Update';
        ModalResult:=mrYes;
        Name:='CancelButton';
        Enabled:=False;
      end;
      Timer.Enabled:=True;
      MsgDlgOnTimer(Self);
      Result:=ShowModal;
    finally
      Timer.Free;
      Free;
    end;
  end;
end;

procedure TfMain.MsgDlgOnTimer(Sender: TObject);
var
  Button: TButton;
begin
  with ARemainMsgDlg do begin
    Caption:=Format(aRemainMsgDlgCaption, [aRemainTime]);
    Button:=TButton((TPanel(FindComponent('Panel'))).FindComponent('YesButton'));
    Button.Caption:=Format(aRemainBtnCaption, [aRemainTime]);
  end;
  if (aRemainTime = 0) then
    ARemainMsgDlg.ModalResult:=ID_IGNORE;
  Dec(aRemainTime);
end;

procedure TfMain.Timer1Timer(Sender: TObject);
begin
  CopiedLabel.Visible:=False;
  Timer1.Enabled:=False;
end;

procedure TfMain.Timer2Timer(Sender: TObject);
var
  PingSend: TPINGSend;
  PingTime: Integer;
begin
  if NetMonitoringCheckBox.Checked and device_conn_flag then begin
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

  if device_conn_flag then
    SerialSendByte(hello_cmd)
  else begin
    SerialClose();
    DeactivateInterface();
    Timer2.Enabled:=False;
  end;
  device_conn_flag:=False;
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

procedure TfMain.WaitingTimeTrackBarChange(Sender: TObject);
begin
  WaitingSecLabel.Caption:=IntToStr(WaitingTimeTrackBar.Position * 10) + ' sec';
  if LazSerial1.Active then begin
    SerialSendByte(WaitingTimeTrackBar.Position);
    Logger.Info('Timeout changet on ' + WaitingSecLabel.Caption);
  end;
end;

procedure TfMain.CheckBox7Change(Sender: TObject);
begin
  if CheckBox7.Checked then
    CheckBox8.Enabled:=True
  else
    CheckBox8.Enabled:=False;
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
    RegRunKey(RunEnable)
  else
    RegRunKey(RunDisable);
end;

procedure TfMain.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    Logger.Info('Enabeled logging');
end;

procedure TfMain.CleanLogButtonClick(Sender: TObject);
begin
  ModalResult:=MessageDlg(
    'Clean log file',
    'Are you sure a want to clear log file?',
    mtWarning,
    mbYesNo,
    0
  );
  if ModalResult = mrYes then
    Logger.Clear;
end;

procedure TfMain.DefaultButtonClick(Sender: TObject);
var
  i: Integer;
begin
  ModalResult:=MessageDlg(
    'Default app settings',
    'Are you sure you want to set the default app settings?',
    mtInformation,
    mbYesNo,
    0
  );
  if ModalResult = mrYes then begin
    {for i:=1 to 8 do
      (FindComponent('CheckBox' + IntToStr(i)) as TCheckBox).Checked:=False;}
    for i:=0 to ComponentCount - 1 do
      if (Components[i] is TCheckBox) and (PageControl1.ActivePage = AppTabSheet) then
        (Components[i] as TCheckBox).Checked:=False;
    WaitingTimeTrackBar.Position:=180;
    PowerModeRadioGroup.ItemIndex:=0;
    ModesRadioGroup.ItemIndex:=1;
    PingTimeoutTrackBar.Position:=1000;
    NetAddressEdit.Text:=PingAddressBlank;
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
  i: integer;
begin
  fMain.Caption:=AppName + ' ' + AppVers;
  Application.Title:=AppName + ' ' + AppVers;

  Logger:=TLogger.Create(LogFile);
  NetHint:=HintWindowClass.Create(Self);

  TrayIcon1.Icon:=Application.Icon;
  TrayIcon1.Hint:=AppName + AppVers;

  PageControl1.ActivePageIndex:=0;

  ReadAppConfigs(ConfFile);
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
  if LaunchWithOS then
    CheckBox2.Checked:=True
  else
    RegRunKey(RunDisable);
  if NetAddress <> '' then
    NetAddressEdit.Text:=NetAddress;
  if PingTimeOut > 0 then
    PingTimeoutTrackBar.Position:=PingTimeOut;
  NetMonitoringCheckBox.Checked:=NetMonitoring;

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
    Timer2.Enabled:=False;
    DeactivateInterface();
  end;

  NetHint.Free;

  Logger.Info('Close application');
  Logger.SetQuietMode;

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

  WriteAppConfigs(ConfFile);
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
  response: Integer;
begin { TODO : Очищать буфер во время ожидания RemainMsgDlg }
  response:=LazSerial1.SynSer.RecvByte(0);
  case response of
    $81: begin
      device_conn_flag:=True;

      Logger.Info('Send signal OK');
      IndicatorShape.Brush.Color:=RGBToColor(102, 204, 0); // green
      ButtonsGroupBox.Enabled:=True;
      WaitingTimeGroupBox.Enabled:=True;
      DeviceStatusLabel.Caption:=ds_label + 'connected';

      if check_count > 1 then begin
        FirmwareVersionLabel.Caption:=dfw_label + 'CHINA';
        Logger.Info('Identified device as CHINA firmware');
      end
      else begin
        SerialSendByte(check_device_cmd);
        if china_flag then
          inc(check_count);
      end;

      WaitingTimeTrackBar.OnChange(Self);
    end;
    $80: begin
      china_flag:=False;

      if fw_version > 0 then begin
        Logger.Info('Identified device as firmware MB6718 ' + HexToVerStr(fw_version));
        FirmwareVersionLabel.Caption:=dfw_label + Manufacturer + ' ' + HexToVerStr(fw_version);
        if fw_version < MinFWVers then begin
          basic_func_flag:=False;
          if not basic_accept_flag then begin
            Logger.Info('Firmware version incorrect!');
            Timer2.Enabled:=False;
            case RemainMsgDlg(
              'App warning!',
              'Current device version is ' + HexToVerStr(fw_version) + LineEnding +
              'The application requires a device version ' + HexToVerStr(MinFWVers) +
              ' or higher.' + LineEnding + LineEnding +
              'Will only basic functions be used (automatically)' + LineEnding +
              'or check app update now (recommended)?' + LineEnding,
              mtWarning,
              5
            ) of
              ID_OK, ID_YES: ShowMessage('Pushed by: Update');
              ID_IGNORE: { nop } ; //ShowMessage('Pushed by: Basic');
              ID_CANCEL: { nop } ; //ShowMessage('Pushed by: Cancel')
            end;
            basic_accept_flag:=True;
            Timer2.Enabled:=True;
          end;
        end
        else
          basic_func_flag:=True;
      end
      else begin
        FirmwareVersionLabel.Caption:=dfw_label + Manufacturer;
        SerialSendByte(get_device_version_cmd);
      end;

      if basic_func_flag then begin
        ModesRadioGroup.Enabled:=True;
        PowerModeRadioGroup.Enabled:=True;
        PowerModeRadioGroupClick(Self);
        Sleep(200);
        ModesRadioGroup.OnClick(Self);
      end;
    end;
    $01..$7F: begin
      if check_count < 2 then
        fw_version:=response;
    end;
  end;
end;

procedure TfMain.LazSerial1Status(Sender: TObject; Reason: THookSerialReason;
  const Value: string);
begin
  case Reason of
    HR_SerialClose: begin
      DeviceStatusLabel.Caption:=ds_label + 'Not connected';
      FirmwareVersionLabel.Caption:=dfw_label + 'undefined';
      fw_version:=0;
      check_count:=0;
      china_flag:=True;
      device_conn_flag:=False;
      basic_func_flag:=False;
      basic_accept_flag:=False;
      Logger.Info('Port disconnected');
    end;
    HR_Connect: begin
      DeviceStatusLabel.Caption:=ds_label + 'Try to connect';
      FirmwareVersionLabel.Caption:=dfw_label + 'undefined';
      fw_version:=0;
      check_count:=0;
      china_flag:=True;
      device_conn_flag:=False;
      basic_func_flag:=False;
      basic_accept_flag:=False;
      Logger.Info('Port connected');
    end;
  end;
end;

procedure TfMain.PowerModeRadioGroupClick(Sender: TObject);
begin
  if PowerModeRadioGroup.ItemIndex=1 then begin
    PowerOffButton.Enabled:=True;
    ModesRadioGroup.Controls[2].Enabled:=True;
  end
  else begin
    PowerOffButton.Enabled:=False;
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

procedure TfMain.PowerOffButtonClick(Sender: TObject);
begin
  SerialSendCommand(PowerOffCmd);
end;

procedure TfMain.StartStopButtonClick(Sender: TObject);
begin
  if LazSerial1.Active then begin
    SerialClose();
    Timer2.Enabled:=False;
    DeactivateInterface();
    Logger.Info('Close ' + PortSelectorComboBox.Text + ' port and session');
  end
  else begin
    if SerialOpen(PortSelectorComboBox.Text) then begin
      ActivateInterface();
      SerialSendByte(hello_cmd);
      Timer2.Enabled:=True;
      Logger.Info('Open ' + PortSelectorComboBox.Text + ' port and session');
    end;
  end;
end;

procedure TfMain.SupportEmailLabelClick(Sender: TObject);
begin
  OpenURL('mailto:support@usbwatchdog.ru');
end;

procedure TfMain.WalletsLabelClick(Sender: TObject);
begin
  if (Sender is TLabel) then
    case (Sender as TLabel).Tag of
      1: Clipboard.AsText:=xmr_wallet;
      2: Clipboard.AsText:=eth_wallet;
      3: Clipboard.AsText:=btc_wallet;
    end;
  CopiedLabel.Visible:=True;
  Timer1.Enabled:=True;
end;

procedure TfMain.m10LabelClick(Sender: TObject);
begin
  WaitingTimeTrackBar.Position:=60;
  WaitingTimeTrackBar.OnChange(WaitingTimeTrackBar);
end;

procedure TfMain.m15LabelClick(Sender: TObject);
begin
  WaitingTimeTrackBar.Position:=90;
  WaitingTimeTrackBar.OnChange(WaitingTimeTrackBar);
end;

procedure TfMain.m5LabelClick(Sender: TObject);
begin
  WaitingTimeTrackBar.Position:=30;
  WaitingTimeTrackBar.OnChange(WaitingTimeTrackBar);
end;

procedure TfMain.ModesRadioGroupClick(Sender: TObject);
begin
  if LazSerial1.Active then
    case ModesRadioGroup.ItemIndex of
      0: begin
        Logger.Info('Call "Change on Soft Mode" action');
        SerialSendByte(soft_mode_cmd);
      end;
      1: begin
        Logger.Info('Call "Change on Hard Mode" action');
        SerialSendByte(hard_mode_cmd);
      end;
      2: begin
        Logger.Info('Call "Change on Power OFF Mode" action');
        SerialSendByte(power_off_mode_cmd);
      end;
    end;
end;

procedure TfMain.NetAddressEditClick(Sender: TObject);
begin
  if NetAddressEdit.Text = PingAddressBlank then
    NetAddressEdit.Text:='';
end;

procedure TfMain.NetAddressEditExit(Sender: TObject);
begin
  if NetHint <> nil then
    NetHint.ReleaseHandle;
  if NetAddressEdit.Text = '' then begin
    NetAddressEdit.Text:=PingAddressBlank;
    NetAddressEdit.Font.Color:=clSilver;
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
      ModalResult:=MessageDlg(
        'Network address warning.',
        'No network address entered! Please enter an address.',
        mtWarning,
        [mbOK],
        ''
      );
      NetEditShowHint();
    end
    else
      {if IsIP(NetAddressEdit.Text) or IsIP6(NetAddressEdit.Text) then
        ActivatePingInterface()}
      if ValidURL(NetAddressEdit.Text) then
        ActivatePingInterface()
      else
        NetEditShowHint()
  else
    DeactivatePingInterface();
end;

procedure TfMain.PingTimeoutTrackBarChange(Sender: TObject);
begin
  PingTimeoutSecLabel.Caption:=IntToStr(PingTimeoutTrackBar.Position) + ' ms';
end;

procedure TfMain.s10LabelClick(Sender: TObject);
begin
  WaitingTimeTrackBar.Position:=1;
  WaitingTimeTrackBar.OnChange(WaitingTimeTrackBar);
end;

procedure TfMain.m20LabelClick(Sender: TObject);
begin
  WaitingTimeTrackBar.Position:=120;
  WaitingTimeTrackBar.OnChange(WaitingTimeTrackBar);
end;

procedure TfMain.m3LabelClick(Sender: TObject);
begin
  WaitingTimeTrackBar.Position:=18;
  WaitingTimeTrackBar.OnChange(WaitingTimeTrackBar);
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

function TfMain.SerialOpen(Port: String): Boolean;
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

initialization
  {$R *.lfm}

end.

