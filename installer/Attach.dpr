program Attach;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, UnitPackSystem;

var eStream: TMemoryStream;
    eFiles: TStringList;
begin
  WriteLn('// File attacher for the MM:S installer');
  WriteLn('// by Basic-Master');
  WriteLn('');
  WriteLn('// Looking up files...');
  { Check files }
  if FileExists(ExtractFilePath(ParamStr(0)) + 'files\server.dll') then
    WriteLn('// Found files\server.dll')
  else begin
    WriteLn('// Error: Couldn''t find files\server.dll!');
    ReadLn;
    exit;
  end;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'files\server_i486.so') then
    WriteLn('// Found files\server_i486.so')
  else begin
    WriteLn('// Error: Couldn''t find files\server_i486.so!');
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
  { Compress files }
  WriteLn('// Compressing files...');
  eFiles := TStringList.Create;
  eFiles.Add(ExtractFilePath(ParamStr(0)) + 'files\server.dll');
  eFiles.Add(ExtractFilePath(ParamStr(0)) + 'files\server_i486.so');
  eStream := TMemoryStream.Create;
  CompressFiles(eFiles, ExtractFilePath(ParamStr(0)) + 'temp.zip');
  eStream.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'temp.zip');
  WriteLn('// Attaching output to MMS_Installer.exe...');
  AttachToFile(ExtractFilePath(ParamStr(0)) + 'MMS_Installer.exe', eStream);
  DeleteFile(ExtractFilePath(ParamStr(0)) + 'temp.zip');
  eStream.Free;
  WriteLn('// Done.');
  ReadLn;
end.
