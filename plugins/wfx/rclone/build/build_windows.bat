@echo off
setlocal enabledelayedexpansion

:: Build rclone WFX plugin on Windows
:: Requires Lazarus IDE installed
::
:: Usage:
::   build_windows.bat         Build the plugin
::   build_windows.bat clean   Remove build artifacts

set "SCRIPT_DIR=%~dp0"
set "PLUGIN_DIR=%SCRIPT_DIR%.."
set "SRC_DIR=%PLUGIN_DIR%\src"
set "OUTPUT=%PLUGIN_DIR%\rclone.wfx"
set "LIB_DIR=%PLUGIN_DIR%\lib"

:: Handle clean command
if /i "%1"=="clean" (
    echo Cleaning build artifacts...
    if exist "%LIB_DIR%" rmdir /s /q "%LIB_DIR%"
    if exist "%OUTPUT%" del /q "%OUTPUT%"
    echo Clean complete.
    exit /b 0
)

echo === rclone WFX Plugin Build Script for Windows ===

:: Try lazbuild in PATH
where lazbuild >nul 2>&1
if %ERRORLEVEL% EQU 0 (
    echo Found lazbuild in PATH
    goto :build
)

:: Try common Lazarus installation paths
set "LAZARUS_PATHS=C:\lazarus;C:\Program Files\Lazarus;C:\Program Files (x86)\Lazarus;%LOCALAPPDATA%\lazarus"

for %%P in (%LAZARUS_PATHS%) do (
    if exist "%%P\lazbuild.exe" (
        set "PATH=%%P;%PATH%"
        echo Found Lazarus at %%P
        goto :build
    )
)

:: Not found
echo.
echo ERROR: lazbuild not found.
echo.
echo Please install Lazarus IDE:
echo   1. Download from https://www.lazarus-ide.org/index.php?page=downloads
echo   2. Run the installer
echo   3. Run this script again
echo.
echo Or add Lazarus to your PATH manually.
exit /b 1

:build
echo Building plugin...
cd /d "%SRC_DIR%"
lazbuild --build-mode=Release rclone.lpi
if %ERRORLEVEL% NEQ 0 (
    echo.
    echo Build failed!
    exit /b 1
)

echo.
echo === Build complete ===
echo Output: %OUTPUT%
dir "%OUTPUT%"
