{lib}: let
  importIpkg = path: parseIpkg (builtins.readFile path);

  # Test 1
  test1_text = ''
    package micropack

    version = 0.0.1

    authors = "Stefan Höck"

    brief   = "Minimalistic installer for pack"

    langversion >= 0.6.0

    depends = base >= 0.6.0
            , elab-util

            , idris2
            , parser-toml

    main       = MicroPack
    executable = micropack
    modules = Pack.CmdLn
        , Pack.CmdLn.Completion
        , Pack.CmdLn.Types

        , Pack.Config

    sourcedir  = "src"
  '';

  # Test 1
  test2_text = ''
    package pack

    version = 0.0.1

    authors = "Stefan Höck"

    brief   = "A package manager for Idris2 with curated package collections"

    langversion >= 0.6.0

    -- script to run before building
    prebuild = "bash version.sh"

    postbuild = "bash restore.sh"

    depends = base >= 0.6.0
            , elab-util

            , idris2
            , parser-toml

    modules = Pack.CmdLn
            , Pack.CmdLn.Completion
            , Pack.CmdLn.Opts
            , Pack.CmdLn.Types

            , Pack.Config

    main       = Main

    executable = pack

    sourcedir  = "src"
  '';

  startsWith = prefix: str: let
    prefixLength = builtins.stringLength prefix;
  in
    builtins.substring 0 prefixLength str == prefix;

  parseToLines = text: let
    splitLines = lib.splitString "\n" text;
    trimmedLines = map lib.trim splitLines;
    nonEmptyLines = builtins.filter (line: line != "") trimmedLines;
    nonEmptyLinesWithoutComments = builtins.filter (line: !(startsWith "--" line)) nonEmptyLines;

    foldLines =
      builtins.foldl'
      (
        acc: line: let
          isCommaLine = builtins.substring 0 1 line == ",";
          init = lib.sublist 0 lastIndex acc;
          lastIndex = builtins.length acc - 1;
          last =
            if lastIndex >= 0
            then builtins.elemAt acc lastIndex
            else "";
          newLastLine = last + line;
        in
          if isCommaLine
          then (init ++ [newLastLine])
          else acc ++ [line]
      )
      []
      nonEmptyLinesWithoutComments;
  in
    foldLines;

  parseIpkg = text: let
    lines = parseToLines text;
    # split by first whitespace using regex, first element is the key
    dict =
      builtins.foldl'
      (acc: line: let
        splitByFirstWhitespaceIgnoreEqualSign = builtins.match "([[:alnum:]_]+)[[:space:]]+=?[[:space:]]*(.*)" line;
        key = builtins.elemAt splitByFirstWhitespaceIgnoreEqualSign 0;
        value = builtins.elemAt splitByFirstWhitespaceIgnoreEqualSign 1;

        valueParsed =
          if key == "depends"
          then builtins.map dependecyToHashNameVersion (commaSeparatedToArray value)
          else if key == "modules"
          then commaSeparatedToArray value
          else removeQuotes value;
      in
        acc // {${key} = valueParsed;})
      {}
      lines;

    removeQuotes = value:
      if builtins.substring 0 1 value == "\"" && builtins.substring (builtins.stringLength value - 1) 1 value == "\""
      then builtins.substring 1 (builtins.stringLength value - 2) value
      else value;

    commaSeparatedToArray = value:
      builtins.map lib.trim (lib.splitString "," value);

    dependecyToHashNameVersion = value: let
      split = lib.splitString " " value;
      version = lib.concatStringsSep " " (lib.tail split);
    in {
      name = lib.head split;
      version =
        if version == ""
        then null
        else version;
    };
  in
    dict;

  tests1 = {
    testToLines = {
      expr = parseToLines test1_text;
      expected = [
        "package micropack"
        "version = 0.0.1"
        "authors = \"Stefan Höck\""
        "brief   = \"Minimalistic installer for pack\""
        "langversion >= 0.6.0"
        "depends = base >= 0.6.0, elab-util, idris2, parser-toml"
        "main       = MicroPack"
        "executable = micropack"
        "modules = Pack.CmdLn, Pack.CmdLn.Completion, Pack.CmdLn.Types, Pack.Config"
        "sourcedir  = \"src\""
      ];
    };
    testParsed = {
      expr = parseIpkg test1_text;
      expected = {
        package = "micropack";
        version = "0.0.1";
        authors = "Stefan Höck";
        brief = "Minimalistic installer for pack";
        langversion = ">= 0.6.0";
        depends = [
          {
            name = "base";
            version = ">= 0.6.0";
          }
          {
            name = "elab-util";
            version = null;
          }
          {
            name = "idris2";
            version = null;
          }
          {
            name = "parser-toml";
            version = null;
          }
        ];
        main = "MicroPack";
        executable = "micropack";
        modules = [
          "Pack.CmdLn"
          "Pack.CmdLn.Completion"
          "Pack.CmdLn.Types"
          "Pack.Config"
        ];
        sourcedir = "src";
      };
    };
    test2ToLines = {
      expr = parseToLines test2_text;
      expected = [
        "package pack"
        "version = 0.0.1"
        "authors = \"Stefan Höck\""
        "brief   = \"A package manager for Idris2 with curated package collections\""
        "langversion >= 0.6.0"
        "prebuild = \"bash version.sh\""
        "postbuild = \"bash restore.sh\""
        "depends = base >= 0.6.0, elab-util, idris2, parser-toml"
        "modules = Pack.CmdLn, Pack.CmdLn.Completion, Pack.CmdLn.Opts, Pack.CmdLn.Types, Pack.Config"
        "main       = Main"
        "executable = pack"
        "sourcedir  = \"src\""
      ];
    };
    test2Parsed = {
      expr = parseIpkg test2_text;
      expected = builtins.fromJSON ''{"authors":"Stefan Höck","brief":"A package manager for Idris2 with curated package collections","depends":[{"name":"base","version":">= 0.6.0"},{"name":"elab-util","version":null},{"name":"idris2","version":null},{"name":"parser-toml","version":null}],"executable":"pack","langversion":">= 0.6.0","main":"Main","modules":["Pack.CmdLn","Pack.CmdLn.Completion","Pack.CmdLn.Opts","Pack.CmdLn.Types","Pack.Config"],"package":"pack","postbuild":"bash restore.sh","prebuild":"bash version.sh","sourcedir":"src","version":"0.0.1"}'';
    };
  };

  runTestsAll = tests: let
    testResults = lib.reverseList (lib.debug.runTests tests);
  in
    if testResults == []
    then true
    else let
      errorMessage = lib.concatStringsSep "\n\n" (map (
          test:
            "Test: " + test.name + "\nExpected: " + builtins.toJSON test.expected + "\nActual  : " + builtins.toJSON test.result
        )
        testResults);
    in
      builtins.throw "\nTests failed.\n${errorMessage}\n";
in
  assert (runTestsAll tests1); {
    inherit parseIpkg importIpkg;
  }
