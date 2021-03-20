@echo off
cls
echo === USB WatchDog project build script ===
echo.

:: Имя проекта
set filename=usbwd

:: Вести лог сборки
set log=0
:: Имя лог файла
set logname=build.log

:: Проверяем входной параметр на соответствие одному из ...
if "%1" == "--build-all" (
  call :compil "Release x86"
  call :compil "Release x64"
  exit /b
)
if "%1" == "--build-x86" (
  call :compil "Release x86"
  exit /b
)
if "%1" == "--build-x64" (
  call :compil "Release x64"
  exit /b
)
if "%1" == "--clean" (
  if exist "*.exe" (del /f /q *.exe) else (echo Not finded executable file)
  if exist lib (rd /s /q lib) else (echo Not finded Lib directory)
  if exist %logname% del /f /q %logname%
  echo Project has been successfully cleaned.
  exit /b
)
if "%1" == "--help" (
  echo Usage: make [parameter]
  echo.
  echo Parameters:
  echo   --help        Help or this message.
  echo   --build-all	Build project for all platform.
  echo   --build-x86	Build project for all x86 platform.
  echo   --build-x64	Build project for all x64 platform.
  echo   --clean	Clean project. Remove all temporary, executible and compiled files.
  exit /b
)
echo Please set correct parameter or use --help parameter
exit /b

:: Подпрограмма компиляции
:compil
if %1 == "Release x86" set fprefix=""
if %1 == "Release x64" set fprefix="-x64"
if exist %filename%%fprefix%.exe del /f /q %filename%%fprefix%.exe
if %log% == 1 (
  echo Compiling in progress. See the log file.
  lazbuild %filename%.lpr --build-mode=%1 --verbose --no-write-project > %logname%
) else (
  lazbuild %filename%.lpr --build-mode=%1 --verbose --no-write-project
)
if "%errorlevel%" == "0" (
  if %log% == 1 (
    strip --strip-all --verbose %filename%%fprefix%.exe >> %logname%
  ) else (
    strip --strip-all --verbose %filename%%fprefix%.exe
  )
  echo Project has been successfully builded.
) else (
  if %log% == 1 echo An error occurred in the process. See the log file.
  pause
  exit /b
)
exit /b
