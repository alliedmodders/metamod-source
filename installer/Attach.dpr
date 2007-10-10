program Attach;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, UnitPackSystem;

var eStream: TMemoryStream;
    eFiles: TStringList;
    Version: String;
begin
  WriteLn('// File attacher for the MM:S installer');
  WriteLn('// by Basic-Master');
  WriteLn('');
  WriteLn('// Looking up files...');
  { Check files }
  if FileExists(ExtractFilePath(ParamStr(0)) + 'files\hl2launch.exe') then
    WriteLn('// Found files\hl2launch.exe')
  else begin
    WriteLn('// Error: Couldn''t find files\hl2launch.exe!');
    ReadLn;
    exit;
  end;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'files\server.dll.source') then
    WriteLn('// Found files\server.dll.source')
  else begin
    WriteLn('// Error: Couldn''t find files\server.dll.source!');
    ReadLn;
    exit;
  end;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'files\server_i486.so.source') then
    WriteLn('// Found files\server_i486.so.source')
  else begin
    WriteLn('// Error: Couldn''t find files\server_i486.so.source!');
    ReadLn;
    exit;
  end;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'files\server.dll.orangebox') then
    WriteLn('// Found files\server.dll.orangebox')
  else begin
    WriteLn('// Error: Couldn''t find files\server.dll.orangebox!');
    ReadLn;
    exit;
  end;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'files\server_i486.so.orangebox') then
    WriteLn('// Found files\server_i486.so.orangebox')
  else begin
    WriteLn('// Error: Couldn''t find files\server_i486.so.orangebox!');
    ReadLn;
    exit;
  end;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'MMS_Installer.exe') then
    WriteLn('// Found MMS_Installer.exe')
  else begin
    WriteLn('// Error: Couldn''t find MMS_Installer.exe!');
    ReadLn;
    exit;
  end;
  { Get version number }
  WriteLn('// Please enter the version number:');
  ReadLn(Version);
  if (Trim(Version) = '') then begin
    WriteLn('// Error: Invalid version number!');
    ReadLn;
    exit;
  end;
  { Compress files }
  WriteLn('// Compressing files...');
  eFiles := TStringList.Create;
  eFiles.Add(ExtractFilePath(ParamStr(0)) + 'files\hl2launch.exe');
  eFiles.Add(ExtractFilePath(ParamStr(0)) + 'files\server.dll.source');
  eFiles.Add(ExtractFilePath(ParamStr(0)) + 'files\server_i486.so.source');
  eFiles.Add(ExtractFilePath(ParamStr(0)) + 'files\server.dll.orangebox');
  eFiles.Add(ExtractFilePath(ParamStr(0)) + 'files\server_i486.so.orangebox');
  eStream := TMemoryStream.Create;
  CompressFiles(eFiles, ExtractFilePath(ParamStr(0)) + 'temp.zip');
  eStream.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'temp.zip');
  WriteLn('// Attaching output to MMS_Installer.exe...');
  AttachToFile(ExtractFilePath(ParamStr(0)) + 'MMS_Installer.exe', eStream, Version);
  DeleteFile(ExtractFilePath(ParamStr(0)) + 'temp.zip');
  eStream.Free;
  eFiles.Free;
  WriteLn('// Done.');
  ReadLn;
end.
