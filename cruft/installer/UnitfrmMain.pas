unit UnitfrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TFlatRadioButtonUnit, StdCtrls, ComCtrls, mxFlatControls, JvPageList,
  ExtCtrls, JvExControls, JvComponent, TFlatButtonUnit, jpeg, TFlatEditUnit,
  TFlatGaugeUnit, ImgList, FileCtrl, Registry, CheckLst, TFlatComboBoxUnit,
  TFlatCheckBoxUnit, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdFTP, IdException, IdAntiFreezeBase, IdAntiFreeze,
  IdIntercept, IdLogBase, IdLogFile, pngimage;

type
  TfrmMain = class(TForm)
    jplWizard: TJvPageList;
    jspWelcome: TJvStandardPage;
    pnlButtons: TPanel;
    bvlSpace: TBevel;
    cmdNext: TFlatButton;
    cmdCancel: TFlatButton;
    imgInstall: TImage;
    lblWelcome: TLabel;
    lblInfo1: TLabel;
    lblInfo2: TLabel;
    lblInfo3: TLabel;
    jspLicense: TJvStandardPage;
    pnlLicense: TPanel;
    imgIcon1: TImage;
    lblTitle1: TLabel;
    lblSubTitle1: TLabel;
    freLicense: TmxFlatRichEdit;
    frbAgree: TFlatRadioButton;
    ftbDontAgree: TFlatRadioButton;
    jspInstallMethod: TJvStandardPage;
    pnlHeader2: TPanel;
    imgIcon2: TImage;
    lblTitle2: TLabel;
    lblSubTitle2: TLabel;
    lblInstallMethod: TLabel;
    pnlInstallMethod: TPanel;
    frbDedicatedServer: TFlatRadioButton;
    frbListenServer: TFlatRadioButton;
    frbSelectMod: TFlatRadioButton;
    frbFTP: TFlatRadioButton;
    cmdBack: TFlatButton;
    jspFTP: TJvStandardPage;
    pnlHeader3: TPanel;
    imgIcon3: TImage;
    lblTitle3: TLabel;
    lblSubTitle3: TLabel;
    lblStep1: TLabel;
    pnlFTPData: TPanel;
    lblHost: TLabel;
    txtHost: TFlatEdit;
    lblUserName: TLabel;
    txtUserName: TFlatEdit;
    txtPassword: TFlatEdit;
    lblPassword: TLabel;
    txtPort: TFlatEdit;
    lblPort: TLabel;
    lblStep2: TLabel;
    cmdConnect: TFlatButton;
    pnlDirectory: TPanel;
    trvDirectories: TTreeView;
    lblStep4: TLabel;
    jspInstallProgress: TJvStandardPage;
    pnlHeader5: TPanel;
    imgIcon4: TImage;
    lblTitle5: TLabel;
    lblSubTitle5: TLabel;
    ggeAll: TFlatGauge;
    lblProgress: TLabel;
    ggeItem: TFlatGauge;
    rtfDetails: TmxFlatRichEdit;
    lblDetails: TLabel;
    bvlSpace2: TBevel;
    ilImages: TImageList;
    bvlSpacer1: TBevel;
    bvlSpacer2: TBevel;
    bvlSpacer3: TBevel;
    bvlSpacer5: TBevel;
    jspSelectMod: TJvStandardPage;
    pnlSelectMod: TPanel;
    imgIcon5: TImage;
    lblSelectMod: TLabel;
    lblSelectModInfo: TLabel;
    bvlSelectMod: TBevel;
    lblInfo: TLabel;
    chkPassive: TFlatCheckBox;
    lblStep3: TLabel;
    pnlOS: TPanel;
    optWindows: TFlatRadioButton;
    optLinux: TFlatRadioButton;
    IdFTP: TIdFTP;
    cmdProxySettings: TFlatButton;
    IdAntiFreeze: TIdAntiFreeze;
    frbStandaloneServer: TFlatRadioButton;
    tmrSpeed: TTimer;
    IdLogFile: TIdLogFile;
    shpMods: TShape;
    trvMods: TTreeView;
    FlatRadioButton1: TFlatRadioButton;
    procedure jvwStepsCancelButtonClick(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
    procedure cmdNextClick(Sender: TObject);
    procedure CheckNext(Sender: TObject);
    procedure cmdBackClick(Sender: TObject);
    procedure cmdConnectClick(Sender: TObject);
    procedure jplWizardChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdProxySettingsClick(Sender: TObject);
    procedure txtPortChange(Sender: TObject);
    procedure trvDirectoriesExpanded(Sender: TObject; Node: TTreeNode);
    procedure trvDirectoriesChange(Sender: TObject; Node: TTreeNode);
    procedure IdFTPWork(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrSpeedTimer(Sender: TObject);
    procedure trvDirectoriesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure trvDirectoriesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure trvModsClick(Sender: TObject);
    procedure trvDirectoriesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    OldProgress: Integer;
    CurrProgress: Integer;
  public
    procedure ExceptionHandler(Sender: TObject; E: Exception);
  end;

var
  frmMain: TfrmMain;

var VERSION: String = '<none>';

implementation

uses UnitFunctions, UnitfrmProxy, UnitInstall, UnitSelectModPath;

{$R *.dfm}

procedure TfrmMain.jvwStepsCancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.cmdCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.cmdNextClick(Sender: TObject);
var ePath: String;
    eRegistry: TRegistry;
    eStr: TStringList;
    CurNode: TTreeNode;
    eOS: TOS;
    i: integer;
    Source: Boolean;
begin
  { FTP }
  if jplWizard.ActivePage = jspFTP then begin
    if not IdFTP.Connected then
      IdFTP.Connect;
    eStr := TStringList.Create;
    ePath := '/';
    CurNode := trvDirectories.Selected;
    if (Assigned(CurNode)) then begin
      repeat
        ePath := '/' + CurNode.Text + ePath;
        CurNode := CurNode.Parent;
      until (not Assigned(CurNode));
    end;
    IdFTP.ChangeDir(ePath);
    IdFTP.List(eStr, '', False);
    eStr.CaseSensitive := False;
    // check if gameinfo.txt is in the directory -> valid installation
    if (eStr.IndexOf('gameinfo.txt') = -1) then begin
      MessageBox(Handle, 'Invalid directory. Please select your mod directory and try again.', PChar(Application.Title), MB_ICONWARNING);
      eStr.Free;
      exit;
    end
    else
      eStr.Free;
    // check for orangebox directory
    Source := True;
    if (AnsiSameText(ExtractFileName(trvDirectories.Selected.Text), 'tf')) then begin
      case MessageBox(Handle, 'It looks like your server is using the OrangeBox engine. Would you like to install the appropriate binaries for it?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) of
        mrYes: Source := False;
        mrNo: Source := True;
        mrCancel: begin
          eStr.Free;
          exit;
        end;
      end;
    end;
    // design stuff
    trvDirectories.Enabled := False;
    cmdConnect.Enabled := False;
    optWindows.Enabled := False;
    optLinux.Enabled := False;
    Screen.Cursor := crHourGlass;

    if optWindows.Checked then
      eOS := osWindows
    else
      eOS := osLinux;

    jspInstallProgress.Show;
    // installation
    Screen.Cursor := crAppStart;
    InstallFTP(eOS, Source);
  end
  else if jplWizard.ActivePage = jspInstallProgress then
    Close
  else if jplWizard.ActivePage = jspSelectMod then begin
    { Dedicated Server }
    if frbDedicatedServer.Checked then begin
      Source := True;
      ePath := trvMods.Selected.Text;
      if ePath = 'Counter-Strike:Source' then
        ePath := trvMods.Selected.Parent.Text + '\source dedicated server\cstrike'
      else if ePath = 'Day of Defeat:Source' then
        ePath := trvMods.Selected.Parent.Text + '\source dedicated server\dod'
      else if ePath = 'Half-Life 2 Deathmatch' then
        ePath := trvMods.Selected.Parent.Text + '\source dedicated server\hl2mp'
      else begin
        { get games }
        if ePath = 'Team Fortress 2' then
          ePath := trvMods.Selected.Parent.Text + '\source 2007 dedicated server\tf';
        { ask user, just in case }
        case MessageBox(Handle, 'It looks like your server is using the OrangeBox engine. Would you like to install the appropriate binaries for it?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) of
          mrYes: Source := False;
          mrNo: Source := True;
          mrCancel: exit;
        end;
      end;
      SteamPath := IncludeTrailingPathDelimiter(SteamPath) + 'steamapps\';
      // install it
      if DirectoryExists(SteamPath + ePath) then begin
        jspInstallProgress.Show;
        InstallDedicated(IncludeTrailingPathDelimiter(SteamPath + ePath), True, Source);
      end
      else begin
        MessageBox(Handle, 'Error: The directory of the mod you selected doesn''t exist any more. Run Dedicated Server with the chosen mod and try again.', PChar(Application.Title), MB_ICONERROR);
        jspSelectMod.Show;
        exit;
      end;
    end;
    { Standalone Server }
    if frbStandaloneServer.Checked then begin
      Source := True;
      ePath := trvMods.Selected.Text;
      if ePath = 'Counter-Strike:Source' then
        ePath := 'cstrike'
      else if ePath = 'Day of Defeat:Source' then
        ePath := 'dod'
      else if ePath = 'Half-Life 2 Deathmatch' then
        ePath := 'hl2mp'
      else begin
        { get games }
        if ePath = 'Team Fortress 2' then
          ePath := 'orangebox\tf';
        { ask user, just in case }
        case MessageBox(Handle, 'It looks like your server is using the OrangeBox engine. Would you like to install the appropriate binaries for it?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) of
          mrYes: Source := False;
          mrNo: Source := True;
          mrCancel: exit;
        end;
      end;
      // install it
      if DirectoryExists(StandaloneServer + ePath) then begin
        jspInstallProgress.Show;
        InstallDedicated(IncludeTrailingPathDelimiter(StandaloneServer + ePath), False, Source)
      end
      else begin
        MessageBox(Handle, 'Error: The directory of the mod you selected doesn''t exist (any more). Run Half-Life Dedicated Server with the chosen mod again and restart.', PChar(Application.Title), MB_ICONERROR);
        jspSelectMod.Show;
        exit;
      end;
    end;
    { Listen Server }
    if frbListenServer.Checked then begin
      Source := True;
      ePath := trvMods.Selected.Text;
      if ePath = 'Counter-Strike:Source' then
        ePath := SteamPath + 'SteamApps\' + trvMods.Selected.Parent.Text + '\counter-strike source\cstrike'
      else if ePath = 'Half-Life 2 Deathmatch' then
        ePath := SteamPath + 'SteamApps\' + trvMods.Selected.Parent.Text + '\half-life 2 deathmatch\hl2mp'
      else if ePath = 'Day of Defeat:Source' then
        ePath := SteamPath + 'SteamApps\' + trvMods.Selected.Parent.Text + '\day of defeat source\dod'
      else begin
        { get games }
        if ePath = 'Team Fortress 2' then
          ePath := SteamPath + 'SteamApps\' + trvMods.Selected.Parent.Text + '\team fortress 2\tf';
        { ask user, just in case }
        case MessageBox(Handle, 'It looks like your server is using the OrangeBox engine. Would you like to install the appropriate binaries for it?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) of
          mrYes: Source := False;
          mrNo: Source := True;
          mrCancel: exit;
        end;
      end;

      if Pos(SteamPath, ePath) = 0 then
        MessageBox(Handle, 'An error occured. Please report this bug to the Metamod:Source team and post a new thread on the forums of www.amxmodx.org.', PChar(Application.Title), MB_ICONSTOP)
      else begin
         if not FileExists(ePath + '\gameinfo.txt') then begin
          MessageBox(Handle, 'You have to play this game once before installing Metamod:Source. Do that and try again.', PChar(Application.Title), MB_ICONWARNING);
          exit;
        end;

        jspInstallProgress.Show;
        InstallListen(IncludeTrailingPathDelimiter(ePath), Source);
      end;
    end;
    { Custom mod below }
  end
  else if jplWizard.ActivePage <> jspInstallMethod then
    jplWizard.NextPage
  else begin
    if frbDedicatedServer.Checked then begin    // Dedicated Server
      eRegistry := TRegistry.Create(KEY_READ);
      try
        eRegistry.RootKey := HKEY_CURRENT_USER;
        if eRegistry.OpenKey('Software\Valve\Steam', False) then begin
          ePath := eRegistry.ReadString('SteamPath');
          ePath := IncludeTrailingPathDelimiter(StringReplace(ePath, '/', '\', [rfReplaceAll]));
          SteamPath := ePath;

          ePath := ePath + 'SteamApps\';
          if DirectoryExists(ePath) then begin
            trvMods.Items.Clear;
            // Check Mods
            eStr := GetAllFiles(ePath + '*.*', faDirectory, False, True, False);
            for i := 0 to eStr.Count -1 do begin
              CurNode := trvMods.Items.Add(nil, eStr[i]);

              if DirectoryExists(ePath + eStr[i] + '\source dedicated server\cstrike') then
                trvMods.Items.AddChild(CurNode, 'Counter-Strike:Source');
              if DirectoryExists(ePath + eStr[i] + '\source dedicated server\dod') then
                trvMods.Items.AddChild(CurNode, 'Day of Defeat:Source');
              if DirectoryExists(ePath + eStr[i] + '\source dedicated server\hl2mp') then
                trvMods.Items.AddChild(CurNode, 'Half-Life 2 Deatmatch');
              if DirectoryExists(ePath + eStr[i] + '\source 2007 dedicated server\tf') then
                trvMods.Items.AddChild(CurNode, 'Team Fortress 2');

              if CurNode.Count = 0 then
                CurNode.Free
              else
                CurNode.Expand(False);
            end;
            // Misc
            jspSelectMod.Show;
            trvMods.Selected := nil;
            cmdNext.Enabled := False;
          end
          else
            MessageBox(Handle, 'You have to run Dedicated Server once before installing Metamod:Source!', 'Error', MB_ICONWARNING);
        end
        else
          MessageBox(Handle, 'You haven''t installed Steam yet! Download it at www.steampowered.com, install Dedicated Server and try again.', 'Error', MB_ICONWARNING);
      finally
        eRegistry.Free;
      end;
    end
    else if frbListenServer.Checked then begin  // Listen Server
      eRegistry := TRegistry.Create(KEY_READ);
      try
        eRegistry.RootKey := HKEY_CURRENT_USER;
        if eRegistry.OpenKey('Software\Valve\Steam', False) then begin
          ePath := eRegistry.ReadString('SteamPath');
          ePath := IncludeTrailingPathDelimiter(StringReplace(ePath, '/', '\', [rfReplaceAll]));
          SteamPath := ePath;

          ePath := ePath + 'SteamApps\';
          if DirectoryExists(ePath) then begin
            trvMods.Items.Clear;
            // Check Mods
            eStr := GetAllFiles(ePath + '*.*', faDirectory, False, True, False);
            for i := 0 to eStr.Count -1 do begin
              CurNode := trvMods.Items.Add(nil, eStr[i]);

              if DirectoryExists(ePath + eStr[i] + '\counter-strike source') then
                trvMods.Items.AddChild(CurNode, 'Counter-Strike:Source');
              if DirectoryExists(ePath + eStr[i] + '\day of defeat source') then
                trvMods.Items.AddChild(CurNode, 'Day of Defeat:Source');
              if DirectoryExists(ePath + eStr[i] + '\half-life 2 deathmatch') then
                trvMods.Items.AddChild(CurNode, 'Half-Life 2 Deatmatch');
              if DirectoryExists(ePath + eStr[i] + '\team fortress 2') then
                trvMods.Items.AddChild(CurNode, 'Team Fortress 2');

              if CurNode.Count = 0 then
                CurNode.Free
              else
                CurNode.Expand(False);
            end;
            // Misc
            jspSelectMod.Show;
            trvMods.Selected := nil;
            cmdNext.Enabled := False;
          end
          else
            MessageBox(Handle, 'You haven''t installed Steam yet! Download it at www.steampowered.com, install Dedicated Server and try again.', 'Error', MB_ICONWARNING);
        end
        else
          MessageBox(Handle, 'You haven''t installed Steam yet! Download it at www.steampowered.com, install Dedicated Server and try again.', 'Error', MB_ICONWARNING);
      finally
        eRegistry.Free;
      end;
    end
    else if frbStandaloneServer.Checked then begin // Standalone Server
      eRegistry := TRegistry.Create;
      try
        eRegistry.RootKey := HKEY_CURRENT_USER;
        if eRegistry.OpenKey('Software\Valve\HLServer', False) then begin
          StandaloneServer := IncludeTrailingPathDelimiter(eRegistry.ReadString('InstallPath'));
          if DirectoryExists(StandaloneServer + 'cstrike') then
            trvMods.Items.Add(nil, 'Counter-Strike:Source');
          if DirectoryExists(StandaloneServer + 'dod') then
            trvMods.Items.Add(nil, 'Day of Defeat:Source');
          if DirectoryExists(StandaloneServer + 'hl2mp') then
            trvMods.Items.Add(nil, 'Half-Life 2 Deatmatch');
          if DirectoryExists(StandaloneServer + 'orangebox\tf') then
            trvMods.Items.Add(nil, 'Team Fortress 2');
          jspSelectMod.Show;
          cmdNext.Enabled := False;
        end
        else
          MessageBox(Handle, 'You haven''t installed Half-Life Dedicated Server yet!',  'Error', MB_ICONWARNING);
      finally
        eRegistry.Free;
      end;
    end
    else if frbSelectMod.Checked then begin 
      { Custom mod }
      if frmSelectModPath.ShowModal = mrOk then begin
        ePath := frmSelectModPath.trvDirectory.SelectedFolder.PathName;
        { check if this is an orangebox game }
        Source := True;
        if (AnsiSameText(ExtractFileName(ePath), 'tf')) then begin
          case MessageBox(Handle, 'It looks like your server is using the OrangeBox engine. Would you like to install the appropriate binaries for it?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) of
            mrYes: Source := False;
            mrNo: Source := True;
            mrCancel: exit;
          end;
        end;
        { install now }
        jspInstallProgress.Show;
        InstallCustom(IncludeTrailingPathDelimiter(ePath), osWindows, Source);
      end;
    end
    else if frbFTP.Checked then // FTP
      jspFTP.Show;
  end;
end;

procedure TfrmMain.CheckNext(Sender: TObject);
begin
  cmdNext.Enabled := frbAgree.Checked;
end;

procedure TfrmMain.cmdBackClick(Sender: TObject);
begin
  if jplWizard.ActivePage = jspFTP then
    jspInstallMethod.Show
  else begin
    jplWizard.PrevPage;
    cmdBack.Visible := jplWizard.ActivePageIndex <> 0;
  end;
end;

procedure TfrmMain.cmdConnectClick(Sender: TObject);
var i: integer;
    eStr: TStringList;
    CurNode: TTreeNode;
    Path: String;
begin
  if (Trim(txtHost.Text) = '') or (Trim(txtUsername.Text) = '') then
    MessageBox(Handle, 'Please fill in each field!', PChar(Application.Title), MB_ICONWARNING)
  else if cmdConnect.Caption = 'Connect' then begin
    // ... design stuff ...
    Screen.Cursor := crHourGlass;
    cmdConnect.Enabled := False;
    cmdProxySettings.Enabled := False;
    txtHost.Enabled := False;
    txtPort.Enabled := False;
    txtUsername.Enabled := False;
    txtPassword.Enabled := False;
    chkPassive.Enabled := False;
    cmdConnect.Caption := 'Connecting...';
    // ... set values ...
    IdFTP.Host := txtHost.Text;
    IdFTP.Port := StrToInt(txtPort.Text);
    IdFTP.Username := txtUsername.Text;
    IdFTP.Passive := chkPassive.Checked;
    IdFTP.Password := txtPassword.Text;
    // ... connect and check values etc ...
    try
      IdFTP.Connect(True, 15000);
      // ... get initial directory ...
      Path := IdFTP.RetrieveCurrentDir;
      // ... "fix" path ...
      eStr := TStringList.Create;
      eStr.Text := StringReplace(Path, '/', #13, [rfReplaceAll]);
      for i := eStr.Count -1 downto 0 do begin
        if eStr[i] = '' then
          eStr.Delete(i);
      end;
      if (Copy(Path, Length(Path) -1, 1) <> '/') then
        Path := Path + '/';
      // ... connect successful, change captions ...
      trvDirectories.Enabled := True;
      cmdConnect.Enabled := True;
      cmdConnect.Caption := 'Disconnect';
      // ... change to / and create all the directories ...
      CurNode := nil;
      if (Path <> '/') then begin
        try
          IdFTP.ChangeDir('/');
          with GetAllDirs do begin
            for i := 0 to Count -1 do begin
              if (Assigned(CurNode)) then
                trvDirectories.Items.AddChild(trvDirectories.Items.Add(nil, Strings[i]), 'Scanning...')
              else begin
                CurNode := trvDirectories.Items.Add(nil, Strings[i]);
                trvDirectories.Items.AddChild(CurNode, 'Scanning...');
                if (Pos('/' + CurNode.Text + '/', Path) = 0) then
                  CurNode := nil;
              end
            end;
            Free;
          end;
          IdFTP.ChangeDir(Path);
        except
          if (IdFTP.Connected) then
            IdFTP.ChangeDir(Path)
          else
            IdFTP.Connect;
        end;
      end;
      // ... find directories in start path ...
      if eStr.Count <> 0 then begin
        for i := 0 to eStr.Count -1 do begin
          if (not ((i = 0) and (Assigned(CurNode)))) then
            CurNode := trvDirectories.Items.AddChild(CurNode, eStr[i]);
        end;
      end;
      trvDirectories.Selected := CurNode;
      eStr.Free;
      // ... scan for directories ...
      with GetAllDirs do begin
        for i := 0 to Count -1 do
          trvDirectories.Items.AddChild(trvDirectories.Items.AddChild(CurNode, Strings[i]), 'Scanning...');
        Free;
      end;

      if Assigned(CurNode) then
        CurNode.Expand(False);
    except
      on E: Exception do begin
        // reset button properties
        cmdConnect.Enabled := True;
        txtHost.Enabled := True;
        txtPort.Enabled := True;
        txtUsername.Enabled := True;
        txtPassword.Enabled := True;
        chkPassive.Enabled := True;
        cmdProxySettings.Enabled := True;
        cmdNext.Enabled := False;
        cmdConnect.Caption := 'Connect';
        // analyze messages
        if Pos('Login incorrect.', E.Message) <> 0 then begin // login failed
          MessageBox(Handle, 'Login incorrect. Check your FTP settings and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtUsername.SetFocus;
          txtUsername.SelectAll;
        end
        else if Pos('Host not found.', E.Message) <> 0 then begin // host not found
          MessageBox(Handle, 'The entered host couldn''t be found. Check your settings and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtHost.SetFocus;
          txtHost.SelectAll;
        end
        else if Pos('Connection refused.', E.Message) <> 0 then begin // wrong port (?)
          MessageBox(Handle, 'The host refused the connection. Check your port and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtPort.SetFocus;
          txtPort.SelectAll;
        end
        else if E is EIdProtocolReplyError then begin // wrong port
          MessageBox(Handle, 'The port you entered is definitely wrong. Check it and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtPort.SetFocus;
          txtPort.SelectAll;
        end
        else
          MessageBox(Handle, PChar(E.Message), PChar(Application.Title), MB_ICONWARNING); // unknown error
        // ... connect failed, leave procedure ...
        Screen.Cursor := crDefault;
        exit;
      end;
    end;

    Screen.Cursor := crDefault;
  end
  else begin
    Screen.Cursor := crHourGlass;
    IdFTP.Quit;
    trvDirectories.Items.Clear;
    trvDirectories.Enabled := False;
    cmdConnect.Enabled := True;
    cmdProxySettings.Enabled := True;
    txtHost.Enabled := True;
    txtPort.Enabled := True;
    txtUsername.Enabled := True;
    txtPassword.Enabled := True;
    chkPassive.Enabled := True;
    cmdConnect.Caption := 'Connect';
    cmdNext.Enabled := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.jplWizardChange(Sender: TObject);
begin
  if (jplWizard.ActivePage = jspInstallProgress) then begin
    cmdNext.Caption := '&Finish';
    cmdNext.Enabled := False;
    cmdBack.Visible := False;
  end
  else begin
    cmdNext.Caption := '&Next >';
    cmdNext.Enabled := True;
    cmdBack.Visible := jplWizard.ActivePageIndex <> 0;
  end;

  if (jplWizard.ActivePage = jspLicense) then
    cmdNext.Enabled := frbAgree.Checked;

  if (jplWizard.ActivePage = jspFTP) then
    cmdNext.Enabled := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if LowerCase(ParamStr(1)) = '-logftp' then begin
    MessageBox(Handle, 'FTP installation will be logged to FTP.log!', PChar(Application.Title), MB_ICONINFORMATION);
    IdLogFile.Filename := ExtractFilePath(ParamStr(0)) + 'FTP.log';
    IdLogFile.Active := True;
  end;

  rtfDetails.Clear;
end;

procedure TfrmMain.cmdProxySettingsClick(Sender: TObject);
begin
  frmProxy.ShowModal;
  // Apply Proxy Settings
  case frmProxy.cboProxy.ItemIndex of
    0: IdFTP.ProxySettings.ProxyType := fpcmNone; // none
    1: IdFTP.ProxySettings.ProxyType := fpcmHttpProxyWithFtp; // HTTP Proxy with FTP
    2: IdFTP.ProxySettings.ProxyType := fpcmOpen; // Open
    3: IdFTP.ProxySettings.ProxyType := fpcmSite; // Site
    4: IdFTP.ProxySettings.ProxyType := fpcmTransparent; // Transparent
    5: IdFTP.ProxySettings.ProxyType := fpcmUserPass; // User (Password)
    6: IdFTP.ProxySettings.ProxyType := fpcmUserSite; // User (Site)
  end;

  IdFTP.ProxySettings.Host := frmProxy.txtHost.Text;
  IdFTP.ProxySettings.UserName := frmProxy.txtPort.Text;
  IdFTP.ProxySettings.Password := frmProxy.txtPassword.Text;
  IdFTP.ProxySettings.Port := StrToInt(frmProxy.txtPort.Text);
end;

procedure TfrmMain.txtPortChange(Sender: TObject);
var i: integer;
begin
  if txtPort.Text = '' then
    txtPort.Text := '21'
  else begin
    // check if value is numeric...
    for i := Length(txtPort.Text) downto 1 do begin
      if Pos(txtPort.Text[i], '0123456789') = 0 then begin
        txtPort.Text := '21';
        txtPort.SelStart := 4;
        exit;
      end;
    end;
  end;
end;

procedure TfrmMain.trvDirectoriesExpanded(Sender: TObject;
  Node: TTreeNode);
var ePath: String;
    CurNode: TTreeNode;
    i: integer;
begin
  if Node.Item[0].Text = 'Scanning...' then begin // no directories added yet
    Screen.Cursor := crHourGlass;
    // get complete path
    ePath := '/';
    CurNode := Node;
    repeat
      ePath := '/' + CurNode.Text + ePath;
      CurNode := CurNode.Parent;
    until (not Assigned(CurNode));
    // change dir and add directories in it
    try
      Repaint;
      IdFTP.ChangeDir(ePath);
      with GetAllDirs do begin
        Node.Item[0].Free;
        for i := 0 to Count -1 do begin
          trvDirectories.Items.AddChild(trvDirectories.Items.AddChild(Node, Strings[i]), 'Scanning...');
        end;
        Free;
      end;
    finally
      Application.ProcessMessages;
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.trvDirectoriesChange(Sender: TObject; Node: TTreeNode);
begin
  cmdNext.Enabled := Assigned(trvDirectories.Selected);
end;

procedure TfrmMain.IdFTPWork(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
begin
  if AWorkCount > 15 then begin
    ggeItem.Progress := AWorkCount;
    CurrProgress := AWorkCount;
  end;

  if Cancel then
    IdFTP.Abort;
    
  Application.ProcessMessages;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (jplWizard.ActivePage = jspFTP) and (IdFTP.Connected) then
    IdFTP.Quit;
  if (jplWizard.ActivePage = jspInstallProgress) and (ggeAll.Progress <> ggeAll.MaxValue) and (not Cancel) then begin
    if MessageBox(Handle, 'Do you really want to cancel the installation?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then begin
      Screen.Cursor := crDefault;
      Application.OnException := ExceptionHandler;
      Cancel := True;
      if IdFTP.Connected then
        IdFTP.Quit;
    end
    else
      Action := caNone;
  end;
end;

procedure TfrmMain.ExceptionHandler(Sender: TObject; E: Exception);
begin
  // IF any exceptions were raised after close, nobody would want them so leave this empty
end;

procedure TfrmMain.tmrSpeedTimer(Sender: TObject);
begin
  Caption := CalcSpeed(OldProgress, CurrProgress);
  OldProgress := CurrProgress;
end;

procedure TfrmMain.trvDirectoriesExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  Node.ImageIndex := 1;
  Node.SelectedIndex := 1;
end;

procedure TfrmMain.trvDirectoriesCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Node.ImageIndex := 0;
  Node.SelectedIndex := 0;
end;

procedure TfrmMain.trvModsClick(Sender: TObject);
begin
  if Assigned(trvMods.Selected) then
    cmdNext.Enabled := Assigned(trvMods.Selected.Parent)
  else
    cmdNext.Enabled := False;
end;

procedure TfrmMain.trvDirectoriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Node: TTreeNode;
begin
  Node := trvDirectories.GetNodeAt(X, Y);
  if (Assigned(Node)) then begin
    if (Node.DisplayRect(True).Right < X) then
      trvDirectories.Selected := nil;
  end;
end;

end.
