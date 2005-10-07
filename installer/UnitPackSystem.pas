unit UnitPackSystem;

interface

uses SysUtils, Classes, Zlib;

procedure CompressFiles(Files : TStrings; const Filename : String);
function DecompressStream(Stream : TMemoryStream; DestDirectory : String): Boolean;
function AttachToFile(const AFileName: string; MemoryStream: TMemoryStream): Boolean;
function LoadFromFile(const AFileName: string; MemoryStream: TMemoryStream): Boolean;
procedure Unpack;

implementation

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
function DecompressStream(Stream : TMemoryStream; DestDirectory : String): Boolean;
var
  dest,s : String;
  decompr : TDecompressionStream;
  outfile : TFilestream;
  i,l,c : Integer;
begin
  // IncludeTrailingPathDelimiter (D6/D7 only)
  dest := IncludeTrailingPathDelimiter(DestDirectory);

  Result := False;
  try
    { number of files }
    Stream.Read(c,SizeOf(c));
    for i := 1 to c do
    begin
      { read filename }
      Stream.Read(l,SizeOf(l));
      SetLength(s,l);
      Stream.Read(s[1],l);
      { read filesize }
      Stream.Read(l,SizeOf(l));
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
    end;
  finally
    Result := True;
  end;
end;

function AttachToFile(const AFileName: string; MemoryStream: TMemoryStream): Boolean;
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
  finally
    aStream.Free;
  end;
  Result := True;
end;

function LoadFromFile(const AFileName: string; MemoryStream: TMemoryStream): Boolean;
var
  aStream: TFileStream;
  iSize: Integer;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  try
    aStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
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

procedure Unpack;
var eStream: TMemoryStream;
begin
  eStream := TMemoryStream.Create;
  try
    LoadFromFile(ParamStr(0), eStream); // Get ZIP
    DecompressStream(eStream, ExtractFilePath(ParamStr(0))); // Unpack files
  except
    raise Exception.Create('No files attached!'); 
  end;
  eStream.Free;
end;

end.
