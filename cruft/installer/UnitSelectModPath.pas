unit UnitSelectModPath;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, FileCtrl, ComCtrls, ShellCtrls,
  TFlatComboBoxUnit, TFlatButtonUnit;

type
  TfrmSelectModPath = class(TForm)
    pnlDesign: TPanel;
    lblInfo: TLabel;
    trvDirectory: TShellTreeView;
    cmdOK: TFlatButton;
    cmdCancel: TFlatButton;
  end;

var
  frmSelectModPath: TfrmSelectModPath;

implementation

{$R *.DFM}

end.
