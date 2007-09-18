program MMS_Installer;

{

  Metamod:Source installer is published under the zlib/libpng license, as well
  as Metamod:Source itself. You can find a copy of it below.

  ---------

  zLib/libpng License

  Copyright (c) 2007, Metamod:Source Development Team

  This software is provided 'as-is', without any express or implied warranty.
  In no event will the authors be held liable for any damages arising from the
  use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it freely,
  subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in a
    product, an acknowledgment in the product documentation would be appreciated
    but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.

  ---------

  Used components:

  - Indy 9 (www.indyproject.org)
  - FlatStyle Components (www.torry.net)
  - FlatPack Component Pack (www.torry.net)
  - JVCL Lib Pack 3.0 (jvcl.sourceforge.net)

  Half-Life 2 Icons by Vasili "vaksa" Vorotnikov, Thanks!
  Visit www.vaksa.net for further information

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
