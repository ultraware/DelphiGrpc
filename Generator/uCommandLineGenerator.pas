unit uCommandLineGenerator;

interface


{ keys:
  /h /help - print help page
  /f / file - input file or folder name. Can be used several times.
  folder name should ends with '\'. If so, generator will search
  for all *.proto files in this folder
  /o /output - output folder. Folder must have write access rights.
  /cc /customclient to use custom client unit / class
  /cs /customserver to use custom server unit / class
}

const
  InputFileAliases: TArray<string> = ['f', 'file'];
  OutputFolderAliases: TArray<string> = ['o', 'output'];
  HelpAliases: TArray<string> = ['h', 'help'];
  CustomClientAliases: TArray<string> = ['cc', 'customclient'];
  CustomServerAliases: TArray<string> = ['cs', 'customserver'];

procedure ExecuteConsoleGenerator;

implementation

uses uCommandLineParams, System.SysUtils, System.StrUtils, System.Classes,
     uProtoBufGenerator, uProtoBufGenerator.Connector.Base;

procedure PrintHelp;
begin
  Writeln('/h /help  -  print help page');
  Writeln('/f / file -  input file or folder name. Can be used several times.');
  Writeln('             if you use folder name - generator will search');
  Writeln('             for all *.proto files in this folder');
  Writeln('             for parse several .proto files - separate them with space');
  Writeln('             do not forget to enquote paths with "", if them contains spaces');
  Writeln('/o /output - output folder. Folder must have write access rights.');
  Writeln('/cc /customclient to use custom client unit / class');
  Writeln('/cs /customserver to use custom server unit / class');
end;

procedure GenerateSingle(const InputFileOrDir, OutputDir: string);

  procedure GenerateSingleFile(const InputFileName: string);
  begin
    GlobalGeneratorClass.Generate(InputFileName, OutputDir);
  end;

  procedure GenerateDirectory(const Dir: string);
  var
    SR: TSearchRec;
    Res: integer;
  begin
    Res := FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.proto', faAnyFile, SR);
    try
      while Res = 0 do
        begin
          GenerateSingleFile(IncludeTrailingPathDelimiter(Dir) + SR.Name);
          Res := FindNext(SR);
        end;
    finally
      FindClose(SR);
    end;
  end;

begin
   if FileExists(InputFileOrDir) then
      GenerateSingleFile(InputFileOrDir)
   else if DirectoryExists(InputFileOrDir) then
      GenerateDirectory(InputFileOrDir);
end;

procedure GenerateAll(ParamList: TParamList);
  procedure FillInputFileList(SL: TStrings);
  var
    i: integer;
  begin
    i := 0;
    while (ParamList.FindParam(InputFileAliases, i) <> -1) do
      begin
        SL.DelimitedText := SL.DelimitedText + ParamList[i].ParamValue;
        Inc(i);
      end;

    for i := SL.Count - 1 downto 0 do
      if Trim(SL[i]) = '' then
        SL.Delete(i)
      else
        SL[i] := Trim(SL[i]);
  end;

  function CheckFilesExists(SL: TStrings): Boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := 0 to SL.Count - 1 do
      begin
        Result := FileExists(SL[i]) or DirectoryExists(SL[i]); // do not avoid files without '.proto' extencion
        if not Result then
          begin
            Writeln('input file or folder ' + SL[i] + ' does not exists');
            Break;
          end;
      end;

    if SL.Count = 0 then
      begin
        Writeln('input files not specified');
        Result := False;
      end;
  end;

  function FindOutputFolder(out Folder: string): Boolean;
  var
    i: integer;
  begin
    Result := False;
    i := ParamList.FindParam(OutputFolderAliases);
    if i = -1 then
      begin
        Writeln(' /o parameter not specified');
        exit;
      end;
    Folder := StringReplace(ParamList[i].ParamValue, ';', '', [rfReplaceAll]);

    if not DirectoryExists(Folder) then
      begin
        Writeln('output directory ' + Folder + ' does not exists');
        Folder := '';
        exit;
      end;
    Result := True;
  end;

var
  InputFileList: TStringList;
  OutputFolder: string;
  i: integer;
begin
  InputFileList := TStringList.Create;
  try
    InputFileList.Delimiter := ';';
    InputFileList.StrictDelimiter := True;
    FillInputFileList(InputFileList);
    if not CheckFilesExists(InputFileList) then
      Exit;
    if not FindOutputFolder(OutputFolder) then
      Exit;
    for i := 0 to InputFileList.Count - 1 do
      GenerateSingle(InputFileList[i], OutputFolder);
  finally
    InputFileList.Free;
  end;
end;

procedure ExecuteConsoleGenerator;
var
  ParamList: TParamList;
begin
  try
    ParamList := CreateAndFillParamList;
    try
      if ParamList.KeyExists(HelpAliases) then
        PrintHelp
      else
      begin
        TProtoBaseGeneratorOptions.UseCustomClient := ParamList.KeyExists(CustomClientAliases);
        TProtoBaseGeneratorOptions.UseCustomServer := ParamList.KeyExists(CustomServerAliases);
        GenerateAll(ParamList);
      end;
    finally
      ParamList.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ToString);
  end;
end;

end.
