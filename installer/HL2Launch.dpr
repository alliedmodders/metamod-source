program HL2Launch;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  ShellApi,
  Windows,
  Classes;

procedure LaunchFile(eFile, eStartDir, eParams: String);
var eStartInfo: TStartupInfo;
    eProcInfo: TProcessInformation;
begin
   FillChar(eStartInfo, SizeOf(TStartupInfo), 0);
   with eStartInfo do begin
     cb := SizeOf(eStartInfo);
     dwFlags := STARTF_USESHOWWINDOW;
   end;

   if (CreateProcess(nil, PChar(eFile + #32 + eParams), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, PChar(eStartDir), eStartInfo, eProcInfo)) then begin
     try
       WaitForSingleObject(eProcInfo.hProcess, INFINITE);
     finally
       CloseHandle(eProcInfo.hProcess);
       CloseHandle(eProcInfo.hThread);
     end;
     SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
     Write(' Done.' + #13#10);
   end
   else begin
     SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 12);
     Write(' Couldn''t start hl2.exe!' + #13#10);
   end;
end;

function GetFileSize(eFile: String): Int64;
var eFindHandle: THandle;
    eFindData: TWIN32FINDDATA;
begin
  Result := 0;
  if not FileExists(eFile) then exit;
  eFindHandle := FindFirstFile(PChar(eFile), eFindData);
  if eFindHandle = INVALID_HANDLE_VALUE then exit;
  Result := (eFindData.nFileSizeHigh * (Int64(MAXDWORD) + 1)) + eFindData.nFileSizeLow;
  FindClose(eFindHandle);
end;

var eStream: TFileStream;
    ePath, eParams: String;
    eModDir: String;
    eSearchRec: TSearchRec;
    eStr: TStringList;
    i: integer;
    CheckSuccessful: Boolean;
    StartTime: Cardinal;
begin
  ePath := ExtractFilePath(ParamStr(0));
  for i := 1 to ParamCount do
    eParams := eParams + #32 + ParamStr(i);
  Delete(eParams, 1, 1);
  if Pos('console', LowerCase(eParams)) = 0 then
    eParams := eParams + ' -console';
  eStream := nil;
  eModDir := '';

  SetConsoleTitle('HL2 Launcher');
  Sleep(200); // wait a few ms until the launch program is closed

  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
  WriteLn(' _   _ _     ____       _                           _               ');
  WriteLn('| | | | |   |___ \     | |    __ _ _   _ _ __   ___| |__   ___ _ __ ');
  WriteLn('| |_| | |     __) |    | |   / _` | | | | ''_ \ / __| ''_ \ / _ \ ''__|');
  WriteLn('|  _  | |___ / __/     | |__| (_| | |_| | | | | (__| | | |  __/ |   ');
  WriteLn('|_| |_|_____|_____|    |_____\__,_|\__,_|_| |_|\___|_| |_|\___|_|   ');
  WriteLn('  for listen servers using Metamod:Source');
  WriteLn('');
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
  { Check files }
  WriteLn('Checking files...');
  if not FileExists(ePath + 'hl2.exe') then begin
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 12);
    WriteLn('Error: hl2.exe is missing! Maybe wrong directory? If not, start your HL2 Mod again via Steam and try again.');
    ReadLn;
    exit;
  end;
  if not FileExists(Copy(ePath, 1, Pos('\steamapps\', LowerCase(ePath))) + 'steam.exe') then begin
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 12);
    WriteLn('Error: Cannot find steam.exe! Make sure this application is located in your listen server''s directory.');
    ReadLn;
    exit;
  end;
  { Verify GameInfo.txt ... }
  Write('Verifying GameInfo.txt...');
  if (FindFirst(ePath + '*.*', faDirectory, eSearchRec) = 0) then begin
    repeat
      if (FileExists(ePath + eSearchRec.Name + '\GameInfo.txt')) then begin
        eModDir := eSearchRec.Name;
        break;
      end;
    until (FindNext(eSearchRec) <> 0);
  end;
  FindClose(eSearchRec.FindHandle);
  if eModDir = '' then begin
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 12);
    WriteLn('');
    WriteLn('Error: Couldn''t find GameInfo.txt!');
    ReadLn;
    exit;
  end
  else begin
    if Pos('game', LowerCase(eParams)) = 0 then // a small test which isn't worth a notice
      eParams := '-game ' + eSearchRec.Name + #32 + eParams;
    
    eStr := TStringList.Create;
    eStr.LoadFromFile(ePath + eModDir + '\GameInfo.txt');
    if Pos('|gameinfo_path|addons/metamod/bin', LowerCase(eStr.Text)) = 0 then begin
      CheckSuccessful := False;
      for i := 0 to eStr.Count -1 do begin
        if Pos('searchpaths', LowerCase(Trim(eStr[i]))) = 1 then begin
          if i+3 >= eStr.Count then
            break;
          eStr.Insert(i+2, '			GameBin				|gameinfo_path|addons/metamod/bin');
          CheckSuccessful := True;
          break;
        end;
      end;

      if CheckSuccessful then begin
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
        SetFileAttributes(PChar(ePath + eModDir + '\GameInfo.txt'), 0);
        eStr.SaveToFile(ePath + eModDir + '\GameInfo.txt');
        SetFileAttributes(PChar(ePath + eModDir + '\GameInfo.txt'), faReadOnly);
        Write(' Registered MM:S sucessfully' + #13#10);
      end
      else begin
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 4);
        Write(' Unexpected EOF, your GameInfo.txt seems to be corrupt' + #13#10);
      end;
    end
    else begin
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
      Write(' Done' + #13#10);
    end;
    eStr.Free;
  end;
  { ... and set it to write-protected }
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
  Write('Setting GameInfo.txt to write-protected...');
  try
    eStream := TFileStream.Create(ePath + eModDir + '\GameInfo.txt', fmShareDenyWrite);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
    Write(' Done.' + #13#10);
  except
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14);
    WriteLn('');
    WriteLn('Warning: Couldn''t set GameInfo.txt to write-protected!');
    eStream := nil;
  end;
  { Launch Steam if not opened }
  ShellExecute(0, 'open', PChar(Copy(ePath, 1, Pos('\steamapps\', LowerCase(ePath))) + 'steam.exe'), nil, PChar(Copy(ePath, 1, Pos('\steamapps\', LowerCase(ePath)))), SW_SHOW);
  { Launch game }
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
  Write('Starting HL2...');
  StartTime := GetTickCount;
  LaunchFile(ePath + 'hl2.exe', Copy(ePath, 1, Pos('Steam', ePath)+5), eParams);
  if (GetTickCount - StartTime < 10000) then begin
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14);
    WriteLn('Important: If you experience any problems starting HL2 using this program, please start it once via Steam and try again.');
    ReadLn;
  end;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
  { Free GameInfo.txt }
  Write('Removing read-only again from GameInfo.txt...');
  if Assigned(eStream) then begin
    eStream.Free;
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
    Write(' Done' + #13#10);
  end
  else begin
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 4);
    Write(' Skipped' + #13#10);
  end;
  { End message }
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
  WriteLn('');
  WriteLn('Thanks for using Metamod:Source! Visit http://www.sourcemm.net/');
  Sleep(2500);
end.
