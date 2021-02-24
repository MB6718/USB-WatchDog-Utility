@echo off
cls
echo === USB WatchDog project Make script ===
echo.

:: Блок переменных скрипта
set filename=usbwd

:: Проверяем входной параметр на соответствие одному из
if "%1" == "--build-all" (
  call :Build-x86
  call :Build-x64
  exit /b
)
if "%1" == "--build-x86" (
  call :Build-x86
  exit /b
)
if "%1" == "--build-x64" (
  call :Build-x64
  exit /b
)
if "%1" == "--clean" (
  del /f /q *.exe
  rd /s /q lib
  echo Project sucefull clean.
  exit /b
)
if "%1" == "--help" (
  echo Usage: make [parameter]
  echo.
  echo [Parameters:]
  echo   --help	Help or this message.
  echo   --build-all	Build project for all platform.
  echo   --build-x86	Build project for all x86 platform.
  echo   --build-x64	Build project for all x64 platform.
  echo   --clean	Clean project. Remove all temporary, executible and compiled files.
  exit /b
)
echo Please set correct parameter or use --help parameter
exit /b

:: Подпрограмма компиляции под платформу x86
:Build-x86
if exist %filename%.exe del /f /q %filename%.exe
lazbuild %filename%.lpr --build-mode="Release x86" --verbose --no-write-project
if "%errorlevel%" == "0" (
  strip --strip-all --verbose %filename%.exe
) else (
  pause
  exit /b
)
exit /b

:: Подпрограмма компиляции под платформу x64
:Build-x64
if exist %filename%.exe del /f /q %filename%-x64.exe
lazbuild %filename%.lpr --build-mode="Release x64" --verbose --no-write-project
if "%errorlevel%" == "0" (
  strip --strip-all --verbose %filename%-x64.exe
) else (
  pause
  exit /b
)
exit /b
