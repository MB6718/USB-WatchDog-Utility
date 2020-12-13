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
    PowerOffButton: TButton;
    ReScanButton: TButton;
    SoftResetButton: TButton;
    StartStopButton: TBitBtn;
  private

  public

  end;

var
  fMain: TfMain;

implementation

initialization
  {$R *.lfm}

end.

