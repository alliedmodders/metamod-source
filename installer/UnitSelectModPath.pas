unit UnitSelectModPath;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, FileCtrl, ComCtrls, ShellCtrls,
  TFlatComboBoxUnit, TFlatButtonUnit, TFlatCheckBoxUnit;

type
  TfrmSelectModPath = class(TForm)
    pnlDesign: TPanel;
    lblInfo: TLabel;
    trvDirectory: TShellTreeView;
    cmdOK: TFlatButton;
    cmdCancel: TFlatButton;
    chkUsesOrangebox: TFlatCheckBox;
    procedure trvDirectoryClick(Sender: TObject);
  end;

var
  frmSelectModPath: TfrmSelectModPath;

implementation

{$R *.DFM}

procedure TfrmSelectModPath.trvDirectoryClick(Sender: TObject);
begin
  // !! OrangeBox Check !!
  if (trvDirectory.Selected <> nil) then
    chkUsesOrangebox.Checked := (chkUsesOrangebox.Checked) or (trvDirectory.Selected.Text = 'tf');
end;

end.
