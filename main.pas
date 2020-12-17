unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, Clipbrd, LazSerial, LazSynaSer;

type

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
    WaitingSecLabel: TLabel;
    WaitingTimeGroupBox: TGroupBox;
    WaitingTimeTrackBar: TTrackBar;
    XMRWalletLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure m10LabelClick(Sender: TObject);
    procedure m15LabelClick(Sender: TObject);
    procedure m20LabelClick(Sender: TObject);
    procedure m3LabelClick(Sender: TObject);
    procedure m5LabelClick(Sender: TObject);
    procedure PowerModeRadioGroupClick(Sender: TObject);
    procedure ReScanButtonClick(Sender: TObject);
    procedure s10LabelClick(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WaitingTimeTrackBarChange(Sender: TObject);
    procedure WalletsLabelClick(Sender: TObject);
    procedure WalletsLabelMouseLeave(Sender: TObject);
    procedure WalletsLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TimeLabelMouseLeave(Sender: TObject);
    procedure TimeLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure ActivateInterface();
    procedure DeactivateInterface();
  public
    const AppName = 'USB WatchDog';
    const AppVers = 'v0.1';
    const xmr_wallet = 'xmr_address';
    const eth_wallet = 'eth_address';
    const btc_wallet = 'btc_address';

    var Serial_Active: Boolean;
  end;

var
  fMain: TfMain;

implementation

{ TfMain }

procedure TfMain.Timer1Timer(Sender: TObject);
begin
  CopiedLabel.Visible:=False;
  Timer1.Enabled:=False;
end;

procedure TfMain.WaitingTimeTrackBarChange(Sender: TObject);
begin
  WaitingSecLabel.Caption:=IntToStr(WaitingTimeTrackBar.Position * 10) + ' sec';
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  fMain.Caption:=AppName + ' ' + AppVers;
  Application.Title:=AppName + ' ' + AppVers;

  PageControl1.ActivePageIndex:=0;
  Serial_Active:=False;

  PortSelectorComboBox.Items.CommaText:=GetSerialPortNames();
  if PortSelectorComboBox.Items.Count > 0 then begin
    PortSelectorComboBox.ItemIndex:=0;
    StartStopButton.Enabled:=True;
    IndicatorShape.Brush.Color:=RGBToColor(221, 0, 0);  // red
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

procedure TfMain.StartStopButtonClick(Sender: TObject);
begin
  if not Serial_Active then begin
    ActivateInterface();
    Serial_Active:=True;
  end
  else begin
    DeactivateInterface();
    Serial_Active:=False;
  end;
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
  IndicatorShape.Brush.Color:=RGBToColor(88, 174, 0); // darkgreen

  ButtonsGroupBox.Enabled:=True;

  WaitingTimeGroupBox.Enabled:=True;
  ModesRadioGroup.Enabled:=True;
  PowerModeRadioGroup.Enabled:=True;
  PowerModeRadioGroupClick(Self);

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
  NetAddressEdit.Enabled:=True;
  if NetMonitoringCheckBox.Checked then begin
    PingTimeoutTrackBar.Enabled:=True;
    PingTimeoutSecLabel.Enabled:=True;
    PingTimeoutLabel.Enabled:=True;
    PingStatusLabel.Enabled:=True;
  end;
end;

initialization
  {$R *.lfm}

end.

