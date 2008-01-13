unit UnitInstall;

interface

uses SysUtils, Classes, Windows, Graphics, Forms, ShellAPI, Controls, Messages,
     TlHelp32, IdFTPCommon, ComCtrls;

type TOS = (osWindows, osLinux);

procedure AddStatus(Text: String; Color: TColor; ShowTime: Boolean = True);
procedure AddDone(Additional: String = '');
procedure AddSkipped;
procedure AddNotFound;
procedure DownloadFile(eFile: String; eDestination: String);

procedure BasicInstallation(ePath: String; SteamInstall, ListenInstall: Boolean; OS: TOS; const Source: Boolean);
procedure InstallDedicated(eModPath: String; const UseSteam, Source: Boolean);
procedure InstallListen(ePath: String; const Source: Boolean);
procedure InstallCustom(ePath: String; eOS: TOS; const Source: Boolean);
procedure InstallFTP(OS: TOS; const Source: Boolean; const ModDir: String);

var StartTime: TDateTime;
    SteamPath: String;
    StandaloneServer: String;
    Cancel: Boolean = False;

implementation

uses UnitfrmMain, UnitfrmProxy, UnitFunctions, UnitPackSystem;

// useful stuff

function InstallTime: String;
begin
  Result := Copy(FormatDateTime('HH:MM:SS', Now - StartTime), 4, 5);
end;

procedure AddStatus(Text: String; Color: TColor; ShowTime: Boolean = True);
begin
  frmMain.rtfDetails.SelStart := Length(frmMain.rtfDetails.Text);
  if ShowTime then begin
    frmMain.rtfDetails.SelAttributes.Color := clBlack;
    if frmMain.rtfDetails.Text = '' then
      frmMain.rtfDetails.SelText := '[' + InstallTime + '] '
    else
      frmMain.rtfDetails.SelText := #13#10 + '[' + InstallTime + '] ';
    frmMain.rtfDetails.SelStart := Length(frmMain.rtfDetails.Text);
  end
  else
    frmMain.rtfDetails.SelText := #13#10;

  frmMain.rtfDetails.SelStart := Length(frmMain.rtfDetails.Text);
  frmMain.rtfDetails.SelAttributes.Color := Color;
  frmMain.rtfDetails.SelText := Text;
  frmMain.rtfDetails.Perform(WM_VSCROLL, SB_BOTTOM, 0);

  frmMain.Repaint;
  Application.ProcessMessages;
end;

procedure AddDone(Additional: String = '');
begin
  frmMain.rtfDetails.SelStart := Length(frmMain.rtfDetails.Text);
  frmMain.rtfDetails.SelAttributes.Color := clGreen;
  if Additional = '' then
    frmMain.rtfDetails.SelText := ' Done.'
  else
    frmMain.rtfDetails.SelText := ' Done, ' + Additional + '.';
  frmMain.rtfDetails.Perform(WM_VSCROLL, SB_BOTTOM, 0);

  frmMain.Repaint;
  Application.ProcessMessages;
end;

procedure AddSkipped;
begin
  frmMain.rtfDetails.SelStart := Length(frmMain.rtfDetails.Text);
  frmMain.rtfDetails.SelAttributes.Color := $004080FF; // orange
  frmMain.rtfDetails.SelText := ' Skipped.';
  frmMain.rtfDetails.Perform(WM_VSCROLL, SB_BOTTOM, 0);

  frmMain.Repaint;
  Application.ProcessMessages;
end;

procedure AddNotFound;
begin
  frmMain.rtfDetails.SelStart := Length(frmMain.rtfDetails.Text);
  frmMain.rtfDetails.SelAttributes.Color := clRed;
  frmMain.rtfDetails.SelText := ' Not found.';
  frmMain.rtfDetails.Perform(WM_VSCROLL, SB_BOTTOM, 0);

  frmMain.Repaint;
  Application.ProcessMessages;
end;

procedure FileCopy(Source, Destination: String; CopyConfig: Boolean; AddStatus: Boolean = True);
begin
  if (not CopyConfig) and (Pos('config', Source) <> 0) then begin
    if AddStatus then
      AddSkipped;
    exit;
  end;

  if not FileExists(Source) then begin
    if AddStatus then
      AddNotFound;
    exit;
  end;

  try
    if FileExists(Destination) then
      DeleteFile(PChar(Destination));
    CopyFile(PChar(Source), PChar(Destination), False);
  except
    Application.ProcessMessages;
  end;

  if AddStatus then
    AddDone;
end;

procedure DownloadFile(eFile: String; eDestination: String);
var TransferType: TIdFTPTransferType;
begin
  // There's only one file to download and it's ASCII :]
  TransferType := ftASCII;
  if frmMain.IdFTP.TransferType <> TransferType then
    frmMain.IdFTP.TransferType := TransferType;
  // download the file
  frmMain.IdFTP.Get(eFile, eDestination, True);
end;

procedure UploadFile(eFile: String; eDestination: String; CopyConfig: Boolean = True);
var TransferType: TIdFTPTransferType;
begin
  if (Pos('config', eFile) > 0) and (not CopyConfig) then begin
    AddSkipped;
    exit;
  end;
  
  eDestination := StringReplace(eDestination, '\', '/', [rfReplaceAll]);

  // the same as in DownloadFile()
  TransferType := ftBinary;
  if ExtractFileExt(LowerCase(eFile)) = '.txt' then TransferType := ftASCII;
  if frmMain.IdFTP.TransferType <> TransferType then
    frmMain.IdFTP.TransferType := TransferType;
  // upload the file
  frmMain.IdFTP.Put(eFile, eDestination);
  AddDone;
end;

procedure FTPMakeDir(eDir: String);
begin
  eDir := StringReplace(eDir, '\', '/', [rfReplaceAll]);
  try
    frmMain.IdFTP.MakeDir(eDir);
  except
    Application.ProcessMessages;
  end;
end;

function FSize(eFile: String): Cardinal;
var eRec: TSearchRec;
begin
  if FindFirst(eFile, faAnyFile, eRec) = 0 then
    Result := eRec.Size
  else
    Result := 0;
end;

// stuff for killing processes

function GetProcessID(sProcName: String): Integer;
var
  hProcSnap: THandle;
  pe32: TProcessEntry32;
begin
  result := -1;
  hProcSnap := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
  if hProcSnap = INVALID_HANDLE_VALUE then
    exit;

  pe32.dwSize := SizeOf(ProcessEntry32);
  if Process32First(hProcSnap, pe32) = true then begin
    while Process32Next(hProcSnap, pe32) = true do begin
      if pos(sProcName, pe32.szExeFile) <> 0then
        result := pe32.th32ProcessID;
    end;
  end;
  CloseHandle(hProcSnap);
end; 

procedure KillProcess(dwProcID: DWORD); 
var 
  hProcess : Cardinal; 
  dw       : DWORD; 
begin 
  hProcess := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, dwProcID);
  TerminateProcess(hProcess, 0);
  dw := WaitForSingleObject(hProcess, 5000);
  case dw of 
    WAIT_TIMEOUT: begin
      CloseHandle(hProcess);
      exit;
    end;
    WAIT_FAILED: begin 
      RaiseLastOSError; 
      CloseHandle(hProcess); 
      exit; 
    end; 
  end; 
  CloseHandle(hProcess); 
end; 

// Installation here

{ Basic Installation }   

procedure BasicInstallation(ePath: String; SteamInstall, ListenInstall: Boolean; OS: TOS; const Source: Boolean);
var eStr: TStringList;
    CopyConfig: Boolean;
begin
  frmMain.ggeAll.MaxValue := 8;
  frmMain.ggeAll.Progress := 0;
  frmMain.ggeItem.MaxValue := 1;
  frmMain.ggeItem.Progress := 0;

  {if (GetProcessID('Steam.exe') <> -1) and (SteamInstall) then begin
    if MessageBox(frmMain.Handle, 'Steam is still running. It is necersarry to shut it down before you install Metamod:Source. Shut it down now?', PChar(frmMain.Caption), MB_ICONQUESTION + MB_YESNO) = mrYes then begin
      AddStatus('Shutting down Steam...', clBlack, False);
      if GetProcessID('Steam.exe') = -1 then
        AddDone
      else
        KillProcess(GetProcessID('Steam.exe'));
        
      while GetProcessID('Steam.exe') <> -1 do begin // sure is sure...
        Sleep(50);
        Application.ProcessMessages;
      end;
      AddDone;
    end
    else begin
      Application.Terminate;
      exit;
    end;
  end;}
  frmMain.ggeAll.Progress := 1;
  frmMain.ggeItem.Progress := 1;
  { Unpack }
  frmMain.ggeItem.Progress := 0;
  AddStatus('Unpacking files...', clBlack);
  if not Unpack(Source) then begin
    AddStatus('No files attached!', clRed);
    Screen.Cursor := crDefault;
    exit;
  end;
  AddDone;
  frmMain.ggeAll.Progress := 2;
  frmMain.ggeItem.Progress := 1;
  { Check for installation / Create directories }
  CopyConfig := True;
  AddStatus('Creating directories...', clBlack);
  if DirectoryExists(ePath + 'addons\metamod\bin') then begin
    case MessageBox(frmMain.Handle, 'A Metamod:Source installation was already detected. If you choose to reinstall, your configuration files will be erased. Click Yes to continue, No to Upgrade, or Cancel to abort the install.', PChar(frmMain.Caption), MB_ICONQUESTION + MB_YESNOCANCEL) of
      mrNo: CopyConfig := False;
      mrCancel: begin
        Application.Terminate;
        exit;
      end;
    end;
    AddSkipped;
  end
  else begin
    frmMain.ggeItem.Progress := 0;
    ForceDirectories(ePath + 'addons\metamod\bin');
    AddDone;
  end;
  frmMain.ggeItem.Progress := 1;
  frmMain.ggeAll.Progress := 3;

  { gameinfo.txt for check / create VDF file }
  if not FileExists(ePath + 'gameinfo.txt') then begin
    if MessageBox(frmMain.Handle, 'The file "gameinfo.txt" couldn''t be found. Continue installation?', PChar(frmMain.Caption), MB_ICONQUESTION + MB_YESNO) = mrNo then begin
      AddStatus('Installation canceled by user!', clRed, False);
      Screen.Cursor := crDefault;
      Cancel := True;
      exit;
    end;
  end
  else begin
    eStr := TStringList.Create;
    { Metaplugins.ini }
    frmMain.ggeItem.Progress := 0;
    AddStatus('Creating metaplugins.ini...', clBlack);
    if CopyConfig then begin
      eStr.SaveToFile(ePath + 'addons\metamod\metaplugins.ini');
      AddDone;
    end
    else
      AddSkipped;
    frmMain.ggeItem.Progress := 1;
    frmMain.ggeAll.Progress := 4;
    { CDF Plugin }
    frmMain.ggeItem.Progress := 0;
    AddStatus('Creating VDF Plugin...', clBlack);
    if (FileExists(ePath + 'addons\metamod.vdf')) then begin
      eStr.LoadFromFile(ePath + 'addons\metamod.vdf');
      if (Pos('server.dll', eStr.Text) <> 0) then
        AddSkipped
      else begin
        eStr.Add('');
        eStr.Add('"Plugin"');
        eStr.Add('{');
        eStr.Add('    "file"      "..\' + GetModName(ePath) + '\addons\metamod\bin\server.dll"');
        eStr.Add('}');
        eStr.SaveToFile(ePath + 'addons\metamod.vdf');
        AddDone;
      end;
    end
    else begin
      eStr.Add('');
      eStr.Add('"Plugin"');
      eStr.Add('{');
      eStr.Add('    "file"      "..\' + GetModName(ePath) + '\addons\metamod\bin\server.dll"');
      eStr.Add('}');
      eStr.SaveToFile(ePath + 'addons\metamod.vdf');
      AddDone;
    end;
    eStr.Free;
    frmMain.ggeItem.Progress := 1;
    frmMain.ggeAll.Progress := 5;
  end;
  { Copy files }
  frmMain.ggeItem.Progress := 0;
  AddStatus('Copying server.dll...', clBlack);
  CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'server.dll'), PChar(ePath + 'addons\metamod\bin\server.dll'), False);
  AddDone;
  frmMain.ggeItem.Progress := 1;
  frmMain.ggeAll.Progress := 6;
  { Remove files }
  frmMain.ggeItem.Progress := 0;
  AddStatus('Removing temporary files...', clBlack);
  DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'server.dll'));
  DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'server_i486.so'));
  AddDone;
  frmMain.ggeItem.Progress := 1;
  frmMain.ggeAll.Progress := 8;
  { Finish }
  AddStatus('', clBlack, False);
  AddStatus('Finished installation!', clBlack, False);
  frmMain.cmdNext.Enabled := True;
  frmMain.cmdCancel.Hide;
  Screen.Cursor := crDefault;
end;

{ Dedicated Server }

procedure InstallDedicated(eModPath: String; const UseSteam, Source: Boolean);
begin
  StartTime := Now;
  Screen.Cursor := crHourGlass;
  AddStatus('Starting Metamod:Source installation on dedicated server...', clBlack, False);
  BasicInstallation(eModPath, UseSteam, False, osWindows, Source);
end;

{ Listen Server }

procedure InstallListen(ePath: String; const Source: Boolean);
begin
  StartTime := Now;
  Screen.Cursor := crHourGlass;
  AddStatus('Starting Metamod:Source installation on the listen server...', clBlack);
  BasicInstallation(ePath, True, True, osWindows, Source);
end;

{ Custom mod }

procedure InstallCustom(ePath: String; eOS: TOS; const Source: Boolean);
begin
  StartTime := Now;
  Screen.Cursor := crHourGlass;
  AddStatus('Starting Metamod:Source installation...', clBlack);
  BasicInstallation(ePath, False, False, eOS, Source);
end;

{ FTP }

procedure InstallFTP(OS: TOS; const Source: Boolean; const ModDir: String);
function DoReconnect: Boolean;
begin
  Result := False;
  if MessageBox(frmMain.Handle, 'You have been disconnected due to an error. Try to reconnect?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then begin
    try
      frmMain.IdFTP.Connect;
      Result := True;
    except
      MessageBox(frmMain.Handle, 'Failed to reconnect. Installation aborted.', PChar(Application.Title), MB_ICONSTOP);
    end;
  end;
end;


label CreateAgain;
label UploadAgain;
      
var eStr: TStringList;
    CopyConfig, CommentFound: Boolean;
    i: Integer;
begin
  frmMain.cmdCancel.Show;
  frmMain.cmdNext.Hide;
  Screen.Cursor := crHourGlass;

  frmMain.ggeAll.MaxValue := 6;
  frmMain.ggeAll.Progress := 0;
  frmMain.ggeItem.MaxValue := 3;
  frmMain.ggeItem.Progress := 0;

  StartTime := Now;
  { Unpack }
  AddStatus('Unpacking files...', clBlack);
  if not Unpack(Source) then begin
    AddStatus('No files attached!', clRed);
    Screen.Cursor := crDefault;
    exit;
  end;
  AddDone;
  frmMain.ggeAll.Progress := 1;
  frmMain.ggeItem.Progress := 1;
  Sleep(250);
  { Create directories }
  frmMain.ggeAll.Progress := 2;
  frmMain.ggeItem.Progress := 0;

  AddStatus('Creating directories...', clBlack);
  try
    FTPMakeDir('addons');
    frmMain.IdFTP.ChangeDir('addons');
    frmMain.ggeItem.Progress := 1;
    FTPMakeDir('metamod');
    frmMain.IdFTP.ChangeDir('metamod');
    frmMain.ggeItem.Progress := 2;
    FTPMakeDir('bin');
    frmMain.ggeItem.Progress := 3;
    AddDone;
  except
    AddSkipped;
  end;
  { Check VDF Plugin }
  CopyConfig := True;
  
  frmMain.ggeAll.Progress := 3;
  frmMain.ggeItem.Progress := 0;
  
  AddStatus('Creating VDF Plugin...', clBlack);
  eStr := TStringList.Create;
  try
    frmMain.IdFTP.ChangeDirUp;
    
    frmMain.ggeItem.Progress := 1;
    DownloadFile('metamod.vdf', ExtractFilePath(ParamStr(0)) + 'metamod.vdf');
    eStr.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'metamod.vdf');
    frmMain.ggeItem.Progress := 2;
    if (((Pos('server.dll', eStr.Text) <> 0) and (OS = osWindows)) or ((Pos('server_i486.so', eStr.Text) <> 0) and (OS = osLinux))) then begin
      case MessageBox(frmMain.Handle, 'A Metamod:Source installation was already detected. If you choose to reinstall, your configuration files will be erased. Click Yes to continue, No to Upgrade, or Cancel to abort the install.', PChar(frmMain.Caption), MB_ICONQUESTION + MB_YESNOCANCEL) of
        mrNo: CopyConfig := False;
        mrCancel: begin
          Application.Terminate;
          eStr.Free;
          exit;
        end;
      end;
    end;
  except
    // bacon
  end;
  { Create and Upload plugin here }
  frmMain.ggeItem.Progress := 2;
  eStr.Clear;
  eStr.Add('"Plugin"');
  eStr.Add('{');
  if (OS = osWindows) then
    eStr.Add('    "file"      "..\' + ModDir + '\addons\metamod\bin\server.dll"')
  else
    eStr.Add('    "file"      "../' + ModDir + '/addons/metamod/bin/server_i486.so"');
  eStr.Add('}');
  eStr.SaveToFile(ExtractFilePath(ParamStr(0)) + 'metamod.vdf');
  UploadFile(ExtractFilePath(ParamStr(0)) + 'metamod.vdf', 'metamod.vdf');
  frmMain.ggeItem.Progress := 3;
  { Upload metaplugins.ini }
  frmMain.ggeAll.Progress := 4;
  frmMain.ggeItem.MaxValue := 1;
  frmMain.ggeItem.Progress := 0;
  AddStatus('Uploading metaplugins.ini...', clBlack);
  frmMain.IdFTP.ChangeDir('metamod');
  if (CopyConfig) then begin
    eStr.Clear;
    // see http://svn.alliedmods.net/viewvc.cgi/metamodsource/orangebox/addons/metamod/metaplugins.ini?revision=1099&root=Packages
    eStr.Add(';List one plugin per line.  Each line should contain the path to the plugin''s binary.');
    eStr.Add(';Any line starting with a '';'' character is a comment line, and is ignored.');
    eStr.Add(';');
    eStr.Add(';You do not need to include the _i486.so or .dll part of the file name.  Example:');
    eStr.Add('; addons/sourcemod/bin/sourcemod_mm');
    eStr.Add(';You may also put an alias in front of the file, for example:');
    eStr.Add('; sm addons/sourcemod/bin/sourcemod_mm');
    eStr.Add(';Will allow you to use "meta load sm" from the console.');
    eStr.Add(';');
    eStr.Add(';********* LIST PLUGINS BELOW ***********');
    // end
    eStr.SaveToFile(ExtractFilePath(ParamStr(0)) + 'metaplugins.ini');
    UploadFile(ExtractFilePath(ParamStr(0)) + 'metaplugins.ini', 'metaplugins.ini');
  end
  else if (frmMain.IdFTP.Size('metaplugins.ini') <> -1) then begin
    DownloadFile('metaplugins.ini', ExtractFilePath(ParamStr(0)) + 'metaplugins.ini');
    eStr.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'metaplugins.ini');
    CommentFound := False;
    for i := 0 to eStr.Count -1 do begin
      if (Pos(';', eStr[i]) = 1) then begin
        CommentFound := True;
        break;
      end;
    end;
    
    if (CommentFound) then
      AddSkipped
    else begin
      // see http://svn.alliedmods.net/viewvc.cgi/metamodsource/orangebox/addons/metamod/metaplugins.ini?revision=1099&root=Packages
      eStr.Insert(0, ';List one plugin per line.  Each line should contain the path to the plugin''s binary.');
      eStr.Insert(1, ';Any line starting with a '';'' character is a comment line, and is ignored.');
      eStr.Insert(2, ';');
      eStr.Insert(3, ';You do not need to include the _i486.so or .dll part of the file name.  Example:');
      eStr.Insert(4, '; addons/sourcemod/bin/sourcemod_mm');
      eStr.Insert(5, ';You may also put an alias in front of the file, for example:');
      eStr.Insert(6, '; sm addons/sourcemod/bin/sourcemod_mm');
      eStr.Insert(7, ';Will allow you to use "meta load sm" from the console.');
      eStr.Insert(8, ';');
      eStr.Insert(9, ';********* LIST PLUGINS BELOW ***********');
      // end
      eStr.SaveToFile(ExtractFilePath(ParamStr(0)) + 'metaplugins.ini');
      UploadFile(ExtractFilePath(ParamStr(0)) + 'metaplugins.ini', 'metaplugins.ini');
    end;
  end;
  frmMain.ggeAll.Progress := 5;
  frmMain.ggeItem.Progress := 1;
  { Upload server.dll / server_i486.so }
  frmMain.tmrSpeed.Enabled := True;
  frmMain.ggeItem.Progress := 0;
  frmMain.IdFTP.ChangeDir('bin');
  if OS = osWindows then begin
    AddStatus('Uploading server.dll...', clBlack);
    frmMain.ggeItem.MaxValue := FSize(ExtractFilePath(ParamStr(0)) + 'server.dll');
    UploadFile(ExtractFilePath(ParamStr(0)) + 'server.dll', 'server.dll');
  end
  else begin
    AddStatus('Uploading server_i486.so...', clBlack);
    frmMain.ggeItem.MaxValue := FSize(ExtractFilePath(ParamStr(0)) + 'server_i486.so');
    UploadFile(ExtractFilePath(ParamStr(0)) + 'server_i486.so', 'server_i486.so');
  end;
  { Remove created files }
  AddStatus('Removing temporary files...', clBlack);
  DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'server.dll'));
  DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'server_i486.so'));
  DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'metamod.vdf'));
  DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'metaplugins.ini'));
  AddDone;
  { End }
  frmMain.IdFTP.Disconnect;
  frmMain.ggeAll.Progress := frmMain.ggeAll.MaxValue;
  frmMain.ggeItem.Progress := frmMain.ggeItem.MaxValue;
  AddStatus('', clBlack, False);
  AddStatus('Finished installation!', clBlack, False);
  frmMain.tmrSpeed.Enabled := False;
  eStr.Free;

  Screen.Cursor := crDefault;
  frmMain.cmdNext.Enabled := True;
  frmMain.cmdCancel.Hide;
  frmMain.cmdNext.Show;
  frmMain.tmrSpeed.Enabled := False;
  frmMain.Caption := Application.Title;
end;

end.
