unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons;

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
    IndicatorShape: TShape;
    InfoLabel: TLabel;
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
    TitleLabel: TLabel;
    WaitingSecLabel: TLabel;
    WaitingTimeGroupBox: TGroupBox;
    WaitingTimeTrackBar: TTrackBar;
    XMRWalletLabel: TLabel;
  private

  public

  end;

var
  fMain: TfMain;

implementation

{ TfMain }


initialization
  {$R *.lfm}

end.

