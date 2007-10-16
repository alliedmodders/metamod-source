unit UnitPackSystem;

interface

uses SysUtils, Classes, Zlib;

procedure CompressFiles(Files: TStrings; const Filename: String);
function DecompressStream(Stream: TMemoryStream; DestDirectory: String; const Source: Boolean): Boolean;
function AttachToFile(const AFileName: string; MemoryStream: TMemoryStream; Version: String): Boolean;
function LoadFromFile(const AFileName: string; MemoryStream: TMemoryStream): Boolean;
function Unpack(const Source: Boolean): Boolean;
function GetVersion: String;

implementation

uses UnitfrmMain;

procedure CompressFiles(Files : TStrings; const Filename : String);
var
  infile, outfile, tmpFile : TFileStream;
  compr : TCompressionStream;
  i,l : Integer;
  s : String;

begin
  if Files.Count > 0 then
  begin
    outFile := TFileStream.Create(Filename,fmCreate);
    try
      { the number of files }
      l := Files.Count;
      outfile.Write(l,SizeOf(l));
      for i := 0 to Files.Count-1 do
      begin
        infile := TFileStream.Create(Files[i],fmOpenRead);
        try
          { the original filename }
          s := ExtractFilename(Files[i]);
          l := Length(s);
          outfile.Write(l,SizeOf(l));
          outfile.Write(s[1],l);
          { the original filesize }
          l := infile.Size;
          outfile.Write(l,SizeOf(l));
          { compress and store the file temporary}
          tmpFile := TFileStream.Create('tmp',fmCreate);
          compr := TCompressionStream.Create(clMax,tmpfile);
          try
            compr.CopyFrom(infile,l);
          finally
            compr.Free;
            tmpFile.Free;
          end;
          { append the compressed file to the destination file }
          tmpFile := TFileStream.Create('tmp',fmOpenRead);
          try
            l := tmpFile.Size;
            outfile.WriteBuffer(l, SizeOf(l));
            outfile.CopyFrom(tmpFile,0);
          finally
            tmpFile.Free;
          end;
        finally
          infile.Free;
        end;
      end;
    finally
      outfile.Free;
    end;
    DeleteFile('tmp');
  end;
end;

function DecompressStream(Stream : TMemoryStream; DestDirectory : String; const Source: Boolean): Boolean;
var
  dest,s : String;
  decompr : TDecompressionStream;
  outfile : TFilestream;
  i,l,lr,c : Integer;
begin
  // IncludeTrailingPathDelimiter (D6/D7 only)
  dest := IncludeTrailingPathDelimiter(DestDirectory);

  Result := False;
  try
    { number of files }
    Stream.Read(c,SizeOf(c));
    for i := 1 to c do begin
      { read filename }
      Stream.Read(l,SizeOf(l));
      SetLength(s,l);
      Stream.Read(s[1],l);
      Stream.Read(l,SizeOf(l));
      Stream.Read(lr,SizeOf(lr));
      { check if this is the right file }
      if (s = 'hl2launch.exe') or ((Pos('.source', s) <> 0) and (Source)) or ((Pos('.orangebox', s) <> 0) and (not Source)) then begin
        { remove extension and read filesize }
        if (s <> 'hl2launch.exe') then
          s := ChangeFileExt(s, '');
        { decompress the files and store it }
        s := dest+s; //include the path
        outfile := TFileStream.Create(s,fmCreate);
        decompr := TDecompressionStream.Create(Stream);   
        try
          outfile.CopyFrom(decompr,l);
        finally
          outfile.Free;
          decompr.Free;
        end;
      end
      else
        Stream.Position := Stream.Position + lr;
    end;
  finally
    Result := True;
  end;
end;

function AttachToFile(const AFileName: string; MemoryStream: TMemoryStream; Version: String): Boolean;
var
  aStream: TFileStream;
  iSize: Integer;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  try
    aStream := TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyWrite);
    MemoryStream.Seek(0, soFromBeginning);
    // seek to end of File
    // ans Ende der Datei Seeken
    aStream.Seek(0, soFromEnd);
    // copy data from MemoryStream
    // Daten vom MemoryStream kopieren
    aStream.CopyFrom(MemoryStream, 0);
    // save Stream-Size
    // die Streamgröße speichern
    iSize := MemoryStream.Size + SizeOf(Integer);
    aStream.Write(iSize, SizeOf(iSize));
    // save version number+length
    iSize := aStream.Position;
    aStream.Write(Version[1], Length(Version));
    aStream.Write(iSize, SizeOf(iSize));
  finally
    aStream.Free;
  end;
  Result := True;
end;

function LoadFromFile(const AFileName: string; MemoryStream: TMemoryStream): Boolean;
var
  aStream: TMemoryStream;
  iSize: Integer;
  EndPos: Integer;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  try
    aStream := TMemoryStream.Create;
    aStream.LoadFromFile(AFileName);
    // drop version part
    aStream.Seek(-SizeOf(Integer), soFromEnd);
    aStream.Read(EndPos, SizeOf(Integer));
    aStream.SetSize(EndPos);
    // seek to position where Stream-Size is saved
    // zur Position seeken wo Streamgröße gespeichert
    aStream.Seek(-SizeOf(Integer), soFromEnd);
    aStream.Read(iSize, SizeOf(iSize));
    if iSize > aStream.Size then
    begin
      aStream.Free;
      Exit;
    end;
    // seek to position where data is saved
    // zur Position seeken an der die Daten abgelegt sind
    aStream.Seek(-iSize, soFromEnd);
    MemoryStream.SetSize(iSize - SizeOf(Integer));
    MemoryStream.CopyFrom(aStream, iSize - SizeOf(iSize));
    MemoryStream.Seek(0, soFromBeginning);
  finally
    aStream.Free;
  end;
  Result := True;
end;

{ Unpack function }

function Unpack(const Source: Boolean): Boolean;
var eStream: TMemoryStream;
begin
  eStream := TMemoryStream.Create;
  try
    // Get ZIP
    LoadFromFile(ParamStr(0), eStream);
    DecompressStream(eStream, ExtractFilePath(ParamStr(0)), Source); // Unpack files

    Result := True;
  except
    Result := False;
  end;
  eStream.Free;
end;

function GetVersion: String;
var FileStream: TFileStream;
    EndPos, Size: Integer;
    Version: String;
begin
   FileStream := TFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyWrite);
   FileStream.Seek(-SizeOf(Integer), soFromEnd);
   FileStream.Read(EndPos, SizeOf(EndPos));
   FileStream.Position := EndPos;
   Size := FileStream.Size - EndPos - SizeOf(Integer);
   SetString(Result, nil, Size);
   FileStream.Read(Pointer(Result)^, Size); // YAMS
   FileStream.Free;
end;

end.
