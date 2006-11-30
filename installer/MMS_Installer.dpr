program MMS_Installer;

{ Metamod:Source installer ported from the AMX Mod X installer

  Used components:

  - Indy 9 (www.indyproject.org)
  - FlatStyle Components (www.torry.net)
  - FlatPack Component Pack (www.torry.net)
  - JVCL Lib Pack 3.0 (jvcl.sourceforge.net)
  
}

uses
  Forms,
  UnitfrmMain in 'UnitfrmMain.pas' {frmMain},
  UnitFunctions in 'UnitFunctions.pas',
  UnitfrmProxy in 'UnitfrmProxy.pas' {frmProxy},
  UnitInstall in 'UnitInstall.pas',
  UnitSelectModPath in 'UnitSelectModPath.pas' {frmSelectModPath},
  UnitPackSystem in 'UnitPackSystem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Metamod:Source Installer';
  Application.CreateForm(TfrmMain, frmMain);
  UnitfrmMain.VERSION := GetVersion;
  frmMain.lblWelcome.Caption := 'Welcome to the Metamod:Source ' + VERSION + ' Setup Wizard';
  frmMain.lblInfo1.Caption := 'This wizard will guide you through the installation of Metamod:Source ' + VERSION + '.';
  frmMain.lblSubTitle1.Caption := 'Please review the following license terms before installing Metamod:Source ' + VERSION + '.';
  frmMain.lblSelectModInfo.Caption := 'Please select the mod Metamod:Source ' + VERSION + ' shall be installed to.';
  frmMain.lblTitle3.Caption := 'Installing Metamod:Source ' + VERSION + ' via FTP';
  frmMain.lblTitle5.Caption := 'Installing Metamod:Source ' + VERSION;
  frmMain.lblSubTitle5.Caption := 'Please wait while Metamod:Source ' + VERSION + ' is being installed.';
  Application.CreateForm(TfrmProxy, frmProxy);
  Application.CreateForm(TfrmSelectModPath, frmSelectModPath);
  Application.Run;
end.
