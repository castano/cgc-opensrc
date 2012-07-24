# Microsoft Developer Studio Project File - Name="cgc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=CGC - WIN32 DEBUG
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "cgc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "cgc.mak" CFG="CGC - WIN32 DEBUG"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "cgc - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "cgc - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "cgc"
# PROP Scc_LocalPath "."
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "cgc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I ".\common" /I ".\nv2xfp" /I ".\nv3xfp" /I ".\nv3xvp" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /map /debug /machine:I386
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "cgc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I ".\common" /I ".\nv2xfp" /I ".\nv3xfp" /I ".\nv3xvp" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "CGC_DEBUG_THE_COMPILER" /FR /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /map /debug /machine:I386 /pdbtype:sept
# SUBTRACT LINK32 /incremental:no

!ENDIF 

# Begin Target

# Name "cgc - Win32 Release"
# Name "cgc - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\atom.c
# End Source File
# Begin Source File

SOURCE=.\binding.c
# End Source File
# Begin Source File

SOURCE=.\cgcmain.c
# End Source File
# Begin Source File

SOURCE=.\cgstruct.c
# End Source File
# Begin Source File

SOURCE=.\check.c
# End Source File
# Begin Source File

SOURCE=.\compile.c
# End Source File
# Begin Source File

SOURCE=.\constfold.c
# End Source File
# Begin Source File

SOURCE=.\cpp.c
# End Source File
# Begin Source File

SOURCE=.\generic_hal.c
# End Source File
# Begin Source File

SOURCE=.\hal.c
# End Source File
# Begin Source File

SOURCE=.\inline.c
# End Source File
# Begin Source File

SOURCE=.\memory.c
# End Source File
# Begin Source File

SOURCE=.\parser.c
# End Source File
# Begin Source File

SOURCE=.\printutils.c
# End Source File
# Begin Source File

SOURCE=.\scanner.c
# End Source File
# Begin Source File

SOURCE=.\semantic.c
# End Source File
# Begin Source File

SOURCE=.\stdlib.c
# End Source File
# Begin Source File

SOURCE=.\support.c
# End Source File
# Begin Source File

SOURCE=.\support_iter.c
# End Source File
# Begin Source File

SOURCE=.\symbols.c
# End Source File
# Begin Source File

SOURCE=.\tokens.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\atom.h
# End Source File
# Begin Source File

SOURCE=.\binding.h
# End Source File
# Begin Source File

SOURCE=.\check.h
# End Source File
# Begin Source File

SOURCE=.\compile.h
# End Source File
# Begin Source File

SOURCE=.\constfold.h
# End Source File
# Begin Source File

SOURCE=.\cpp.h
# End Source File
# Begin Source File

SOURCE=.\errors.h
# End Source File
# Begin Source File

SOURCE=.\generic_hal.h
# End Source File
# Begin Source File

SOURCE=.\hal.h
# End Source File
# Begin Source File

SOURCE=.\hslversion.h
# End Source File
# Begin Source File

SOURCE=.\inline.h
# End Source File
# Begin Source File

SOURCE=.\memory.h
# End Source File
# Begin Source File

SOURCE=.\parser.h
# End Source File
# Begin Source File

SOURCE=.\printutils.h
# End Source File
# Begin Source File

SOURCE=.\scanner.h
# End Source File
# Begin Source File

SOURCE=.\semantic.h
# End Source File
# Begin Source File

SOURCE=.\slglobals.h
# End Source File
# Begin Source File

SOURCE=.\support.h
# End Source File
# Begin Source File

SOURCE=.\symbols.h
# End Source File
# Begin Source File

SOURCE=.\tokens.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\stdlib.cg

!IF  "$(CFG)" == "cgc - Win32 Release"

# PROP Ignore_Default_Tool 1
USERDEP__STDLI=".\Debug\tokenize.exe"	
# Begin Custom Build
InputPath=.\stdlib.cg

"stdlib.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\Debug\tokenize stdlib.cg >stdlib.c

# End Custom Build

!ELSEIF  "$(CFG)" == "cgc - Win32 Debug"

USERDEP__STDLI=".\Debug\tokenize.exe"	
# Begin Custom Build
InputPath=.\stdlib.cg

"stdlib.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\Debug\tokenize stdlib.cg >stdlib.c

# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
