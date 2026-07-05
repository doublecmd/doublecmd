{
  description = "Double Commander is a free cross platform open source file manager with two panels side by side";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }: let
    version = "1.2.6";

    assets = {
      "x86_64-linux" = {
        file = "doublecmd-${version}.qt.x86_64.tar.xz";
        sha256 = "sha256-JA+bw7hi6ez03XVMq0rPWe0ISvHBX3JveIsjYdrolys=";
      };
      "aarch64-linux" = {
        file = "doublecmd-${version}.qt.aarch64.tar.xz";
        sha256 = "sha256-i4WKRdCRIePwUdytLt9MOKjhkhnXxP4XKnlPlcXjumQ=";
      };
      "x86_64-darwin" = {
        file = "doublecmd-${version}.cocoa.x86_64.dmg";
        sha256 = "sha256-6jrT08WggTYo9keia7vjUolyhuK+laTcwh5B/MQ9FQI=";
      };
      "aarch64-darwin" = {
        file = "doublecmd-${version}.cocoa.aarch64.dmg";
        sha256 = "sha256-EjT9X6miYSK64d4N2opN2gPXlUYoGOKFFzAaB3hIeQI=";
      };
    };

    systems = builtins.attrNames assets;
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);

    projectFor = system: let
      pkgs = nixpkgs.legacyPackages.${system};
      asset = assets.${system};
      isDarwin = pkgs.stdenv.isDarwin;
    in pkgs.stdenv.mkDerivation {
      pname = "doublecmd";
      inherit version;

      src = pkgs.fetchurl {
        url = "https://github.com/doublecmd/doublecmd/releases/download/v${version}/${asset.file}";
        sha256 = asset.sha256;
      };

      sourceRoot = if isDarwin then "dmg-extract" else "doublecmd";

      nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.autoPatchelfHook ]
        ++ pkgs.lib.optionals isDarwin [ pkgs.buildPackages.p7zip ]
        ++ [ pkgs.makeWrapper ];

      buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [
        pkgs.stdenv.cc.cc.lib
        pkgs.qt5.qtbase
        pkgs.xorg.libX11
        pkgs.dbus
        pkgs.glib
      ];

      dontConfigure = true;
      dontBuild = true;

      unpackPhase = if isDarwin then ''
        runHook preUnpack
        mkdir dmg-extract
        7z x $src -odmg-extract -y
        runHook postUnpack
      '' else "";

      installPhase = if isDarwin then ''
        runHook preInstall
        mkdir -p $out/Applications $out/bin
        cp -r "Double Commander/Double Commander.app" $out/Applications/
        chmod +x "$out/Applications/Double Commander.app/Contents/MacOS/doublecmd"
        makeWrapper "$out/Applications/Double Commander.app/Contents/MacOS/doublecmd" $out/bin/doublecmd
        runHook postInstall
      '' else ''
        runHook preInstall
        mkdir -p $out/lib/doublecmd $out/bin
        cp -a * $out/lib/doublecmd/
        chmod +x $out/lib/doublecmd/doublecmd
        makeWrapper $out/lib/doublecmd/doublecmd $out/bin/doublecmd \
          --run "cd $out/lib/doublecmd"
        runHook postInstall
      '';

      meta = with pkgs.lib; {
        description = "Double Commander is a free cross platform open source file manager with two panels side by side";
        homepage = "https://doublecmd.sourceforge.io/";
        downloadPage = "https://github.com/doublecmd/doublecmd/releases";
        license = licenses.gpl2Plus;
        mainProgram = "doublecmd";
        platforms = systems;
        sourceProvenance = [ sourceTypes.binaryNativeCode ];
      };
    };
  in {
    packages = forAllSystems (system: rec {
      doublecmd = projectFor system;
      default = doublecmd;
    });

    apps = forAllSystems (system: let
      doublecmdPkg = projectFor system;
    in {
      doublecmd = {
        type = "app";
        program = "${doublecmdPkg}/bin/doublecmd";
      };
      default = {
        type = "app";
        program = "${doublecmdPkg}/bin/doublecmd";
      };
    });
  };
}
