{
  description = "Flake for building idris2-pack";

  inputs = {
    nixpkgs.url = "github:mattpolzin/nixpkgs/idris2Packages-pack";
    flake-utils.url = "github:numtide/flake-utils";

    filepath.url = "github:stefan-hoeck/idris2-filepath";
    filepath.flake = false;

    idris2-parser.url = "github:stefan-hoeck/idris2-parser";
    idris2-parser.flake = false;

    elab-util.url = "github:stefan-hoeck/idris2-elab-util";
    elab-util.flake = false;

    getopts.url = "github:idris-community/idris2-getopts";
    getopts.flake = false;

    refined.url = "github:stefan-hoeck/idris2-refined";
    refined.flake = false;

    algebra.url = "github:stefan-hoeck/idris2-algebra";
    algebra.flake = false;
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      idris2Packages = pkgs.idris2Packages;
      buildIdris = idris2Packages.buildIdris;
      libs = rec {
        filepath = buildIdris {
          ipkgName = "filepath";
          src = inputs.filepath;
          idrisLibraries = [];
        };

        elab-util = buildIdris {
          ipkgName = "elab-util";
          src = inputs.elab-util;
          idrisLibraries = [];
        };

        getopts = buildIdris {
          ipkgName = "getopts";
          src = inputs.getopts;
          idrisLibraries = [];
        };

        parser = buildIdris {
          ipkgName = "parser";
          src = inputs.idris2-parser;
          idrisLibraries = [elab-util];
        };

        parser-toml = buildIdris {
          ipkgName = "parser-toml";
          src = "${inputs.idris2-parser}/toml";
          idrisLibraries = [parser refined];
        };

        algebra = buildIdris {
          ipkgName = "algebra";
          src = inputs.algebra;
          idrisLibraries = [elab-util];
        };

        refined = buildIdris {
          ipkgName = "refined";
          src = inputs.refined;
          idrisLibraries = [
            elab-util
            algebra
          ];
        };
      };

      inherit (pkgs) lib;
      inherit (import ./ipkg-parser.nix {inherit lib;}) importIpkg;

      packIpkg = importIpkg ./pack.ipkg;
      micropackIpkg = importIpkg ./micropack.ipkg;
      packAdminIpkg = importIpkg ./pack-admin.ipkg;

      # { foo: fooContent, bar: barContent } ["foo", "bar"] => [fooContent, barContent]
      # throw if not found
      depsFromLib = libs: deps:
        map (
          dep:
            if builtins.hasAttr dep libs
            then builtins.getAttr dep libs
            else builtins.throw "Dependency ${dep} not found in ${toString (builtins.attrNames libs)}"
        )
        deps;

      getDepsNamesAndFilterBase = depends:
        lib.filter (name: name != "base" && name != "idris2") (builtins.attrNames depends);

      pack = buildIdris {
        ipkgName = packIpkg.name;

        src = ./.;

        idrisLibraries =
          [
            idris2Packages.idris2Api
          ]
          ++ depsFromLib libs (getDepsNamesAndFilterBase packIpkg.depends);

        meta = with pkgs.lib; {
          description = packIpkg.brief;
          homepage = "https://github.com/stefan-hoeck/idris2-pack";
          license = licenses.bsd3;
          maintainers = [maintainers.mattpolzin];
          platforms = idris2Packages.idris2.meta.platforms;
        };
      };

      micropack = buildIdris {
        ipkgName = micropackIpkg.name;

        src = ./.;

        idrisLibraries =
          [
            idris2Packages.idris2Api
          ]
          ++ depsFromLib libs (getDepsNamesAndFilterBase micropackIpkg.depends);

        meta = with pkgs.lib; {
          description = micropackIpkg.brief;
          homepage = "https://github.com/stefan-hoeck/idris2-pack";
          license = licenses.bsd3;
          maintainers = [maintainers.mattpolzin];
          platforms = idris2Packages.idris2.meta.platforms;
        };
      };

      pack-admin = buildIdris {
        ipkgName = packAdminIpkg.name;

        src = ./.;

        idrisLibraries =
          [
            idris2Packages.idris2Api
          ]
          ++ depsFromLib libs (getDepsNamesAndFilterBase packAdminIpkg.depends);

        meta = with pkgs.lib; {
          description = packAdminIpkg.brief;
          homepage = "https://github.com/stefan-hoeck/idris2-pack";
          license = licenses.bsd3;
          maintainers = [maintainers.mattpolzin];
          platforms = idris2Packages.idris2.meta.platforms;
        };
      };
    in rec {
      packages.pack = pack.executable;
      packages.micropack = micropack.executable;
      packages.pack-admin = pack-admin.executable;
      packages.default = packages.pack;

      # nix build && ./result/bin/pack help
      # nix build .#micropack && ./result/bin/micropack
      # nix build .#pack-admin && ./result/bin/pack-admin help

      # nix develop
      devShell = pkgs.mkShell {
        IDRIS2_PACKAGE_PATH = "${packages.pack.IDRIS2_PACKAGE_PATH}";

        # rm -rdf /tmp/asdf && idris2 --build ./micropack.ipkg --build-dir /tmp/asdf && /tmp/asdf/exec/micropack
        buildInputs = with pkgs; [
          idris2Packages.idris2
          gcc
          gmp
          packages.pack
          packages.micropack
          packages.pack-admin
        ];
      };
    });
}
