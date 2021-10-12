; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{1B1E4F5A-A756-4345-8388-C99E8782B37E}
AppName=MMI
AppVersion=3.0.7
AppPublisher=Bombardier Transportation Sweden AB
AppPublisherURL=http://www.bombardier.com/se/transportation
AppSupportURL=http://www.bombardier.com/se/transportation
AppUpdatesURL=http://www.bombardier.com/se/transportation
DefaultDirName=C:\MMI
DefaultGroupName=MMI
OutputBaseFilename=mmi_setup
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "R:\AOS_IF150_MMI\Distribution\Software\Mmi.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\RelNotes.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\ato_supervised_swe.bmp"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\HiRes_ato_supervised_swe.bmp"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\OtherCar.bmp"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\English.ini"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\Spanish.ini"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\Swedish.ini"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\Distribution\Software\Target\Mmi.ini"; DestDir: "{app}"; Flags: ignoreversion confirmoverwrite
Source: "R:\AOS_IF150_MMI\Distribution\Software\Mmicl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\AOS_IF150_MMI_SW\Inst\Startup.BT"; DestDir: "{drive:{app}}\"; Flags: ignoreversion confirmoverwrite
Source: "R:\AOS_IF150_MMI\AOS_IF150_MMI_SW\Inst\SetMMIAsDefaultShell.reg"; DestDir: "{drive:{app}}\"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\AOS_IF150_MMI_SW\Inst\BTAutoRun\BTAutoRun.Exe"; DestDir: "{drive:{app}}\BTAutoRun"; Flags: ignoreversion
Source: "R:\AOS_IF150_MMI\AOS_IF150_MMI_SW\Inst\BTAutoRun\BTAutoRun.Ini"; DestDir: "{drive:{app}}\BTAutoRun"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Run]
Filename: "{app}\Mmi.exe"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,MMI}"
