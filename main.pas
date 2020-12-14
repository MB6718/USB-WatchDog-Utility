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
    ConnectionGroupBox: TGroupBox;
    DeviceStatusLabel: TLabel;
    FirmwareVersionLabel: TLabel;
    HardResetButton: TButton;
    IndicatorShape: TShape;
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
    SoftResetButton: TButton;
    StartStopButton: TBitBtn;
    WaitingSecLabel: TLabel;
    WaitingTimeGroupBox: TGroupBox;
    WaitingTimeTrackBar: TTrackBar;
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

