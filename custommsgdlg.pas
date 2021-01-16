unit CustomMsgDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Controls, ExtCtrls, Buttons, StdCtrls, Graphics,
  windows;

const
  TimeMark = True;

type

  TCustomMsgDlgButtonInfo = record
    MsgDlgBtn: TMsgDlgBtn;
    Caption: String;
    RemainTimeMark: Boolean;
  end;

  { TMsgDlgEx }

  { TCustomMsgDlg }

  TCustomMsgDlg = class
  public
    constructor Create(const ACaption, AMsg: string; ADlgType: TMsgDlgType;
      AButtons: array of TCustomMsgDlgButtonInfo; AParent: TForm;
      ADefModRes: TModalResult; ATimeLeft: Integer);
    destructor Destroy; override;
    function ShowDialog: TModalResult;
  private
    FMsgDlgForm: TForm;
    FParent: TForm;
    FTimer: TTimer;
    FPanel: TPanel;
    FDefModRes: TModalResult;
    ARemainTime: Integer;
    ARemainButtonMark: TButton;
    ADlgCaption: String;
    ARemainButtonsCaption: String;
    const ARemainCaption = ' (%d)';
    const ARemainDlgCaption = ' Remain: %d sec.';
    procedure DoOnTimer(Sender: TObject);
    function GetTextWidth(aString: string; aFont: TFont; HWND: THandle): Integer;
  end;

function CustomMsgDlgButton(MsgDlgBtn: TMsgDlgBtn; const Caption: String;
  RemainTimeMark: Boolean = False): TCustomMsgDlgButtonInfo;

implementation

const
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);

function CustomMsgDlgButton(MsgDlgBtn: TMsgDlgBtn; const Caption: String;
  RemainTimeMark: Boolean): TCustomMsgDlgButtonInfo;
begin
  Result.MsgDlgBtn:=MsgDlgBtn;
  Result.Caption:=Caption;
  Result.RemainTimeMark:=RemainTimeMark;
end;

constructor TCustomMsgDlg.Create(const ACaption, AMsg: string; ADlgType: TMsgDlgType;
  AButtons: array of TCustomMsgDlgButtonInfo; AParent: TForm;
  ADefModRes: TModalResult; ATimeLeft: Integer);
const
  ButtonsMargin = 10;
  ButtonWidth = 100;
var
  i: Integer;
  StringWidth: Integer;
  Buttons: array of TButton;
  PrevControl: TControl;
begin
  ADlgCaption:=ACaption;
  ARemainTime:=ATimeLeft;
  FDefModRes:=ADefModRes;
  FParent:=AParent;

  FMsgDlgForm:=CreateMessageDialog(AMsg + LineEnding, ADlgType, [mbYes]);
  with FMsgDlgForm do begin
    Color:=clWindow;
    for i:=0 to ControlCount - 1 do
      if Controls[i] is TBitBtn then
        if (Controls[i] as TBitBtn).Caption = '&Yes' then
          (Controls[i] as TBitBtn).Visible:=False;
    if Length(AButtons) > 0 then
      Width:=Length(AButtons) * (ButtonWidth + ButtonsMargin * 2)
    else
      Width:=1 * (ButtonWidth + ButtonsMargin * 2);
    if AMsg <> '' then begin
      StringWidth:=GetTextWidth(AMsg, Font, Handle);
      if (StringWidth + ButtonWidth) > Width then
        Width:=StringWidth + ButtonWidth;
    end;
  end;

  FPanel:=TPanel.Create(FMsgDlgForm);
  with FPanel do begin
    Parent:=FMsgDlgForm;
    ParentColor:=False;
    Color:=clDefault;
    Caption:=' ';
    Align:=alBottom;
    Height:=45;
    BevelInner:=bvLowered;
    BevelOuter:=bvNone;
    Name:='Panel';
  end;

  ARemainButtonMark:=nil;
  PrevControl:=FPanel;
  if Length(AButtons) <= 0 then begin
    AButtons[0]:=CustomMsgDlgButton(mbOk, 'OK');
    SetLength(Buttons, 1);
  end
  else
    SetLength(Buttons, Length(AButtons));
  for i:=Low(Buttons) to High(Buttons) do begin
    Buttons[i]:=TButton.Create(FPanel);
    with Buttons[i] do begin
      Name:='Button' + IntToStr(i);
      Parent:=FPanel;
      Width:=100;
      AnchorVerticalCenterTo(FPanel);
      if PrevControl is TPanel then
        AnchorSide[akRight].Side:=asrRight
      else
        AnchorSide[akRight].Side:=asrLeft;
      AnchorSide[akRight].Control:=PrevControl;
      Anchors:=[akRight, akTop];
      BorderSpacing.Right:=ButtonsMargin;
      ModalResult:=ModalResults[AButtons[i].MsgDlgBtn];
      if AButtons[i].RemainTimeMark then begin
        ARemainButtonMark:=Buttons[i];
        ARemainButtonsCaption:=AButtons[i].Caption + ARemainCaption;
        Font.Style:=[fsBold];
      end;
      Caption:=AButtons[i].Caption;
    end;
    PrevControl:=Buttons[i];
  end;

  FTimer:=TTimer.Create(nil);
  FTimer.Enabled:=False;
  FTimer.Interval:=1000;
  FTimer.OnTimer:=@DoOnTimer;
end;

destructor TCustomMsgDlg.Destroy;
begin
  FTimer.Enabled:=False;
  FTimer.Free;
  FMsgDlgForm.Free;
  inherited Destroy;
end;

function TCustomMsgDlg.ShowDialog: TModalResult;
begin
  with FMsgDlgForm do begin
    FormStyle:=fsSystemStayOnTop;
    if FParent <> nil then begin
      Position:=poDefaultSizeOnly;
      Left:=FParent.Left + (FParent.Width - Width) div 2;
      Top:=FParent.Top + (FParent.Height - Height) div 2;
    end
    else
      Position:=poScreenCenter; //poWorkAreaCenter;
    if ARemainTime > 0 then
      FTimer.Enabled:=True;
    DoOnTimer(Self);
    Result:=ShowModal;
  end;
end;

procedure TCustomMsgDlg.DoOnTimer(Sender: TObject);
begin
  with FMsgDlgForm do begin
    Caption:=Format(ADlgCaption + ARemainDlgCaption, [ARemainTime]);
    if ARemainButtonMark <> nil then
      ARemainButtonMark.Caption:=Format(ARemainButtonsCaption, [ARemainTime]);
    if (ARemainTime = 0) then begin
      FTimer.Enabled:=False;
      ModalResult:=FDefModRes;
    end;
    Dec(ARemainTime);
  end;
end;

function TCustomMsgDlg.GetTextWidth(aString: String; aFont: TFont; HWND: THandle): Integer;
var
  Canvas: TCanvas;
  WordCounter, TempCounter: Integer;
  SubString: String;
begin
  WordCounter:=0;
  SubString:='';
  while Length(aString) > 0 do begin
    TempCounter:=Pos(LineEnding, aString);
    if TempCounter > 0 then begin
      if WordCounter < TempCounter then begin
        WordCounter:=TempCounter - 1;
        SubString:=Copy(aString, 0, TempCounter - 1);
      end;
      aString:=Copy(aString, TempCounter + 2, Length(aString));
    end
    else begin
      if (Length(aString) > 0) and (WordCounter < Length(aString)) then
        SubString:=Copy(aString, 0, Length(aString));
      aString:='';
    end;
  end;

  Canvas:=TCanvas.Create;
  with Canvas do
    try
      Handle:=GetWindowDC(HWND);
      Font:=aFont;
      Result:=TextWidth(SubString);
    finally
      ReleaseDC(HWND, Handle);
      FreeAndNil(Canvas);
    end;
end;

end.
