
set XCFLAGS=/nologo /MT /DWIN32
set XINCLUDE=/I ..\include
set XLIBPATH=/LIBPATH:..\bin

REM z3_stubs.c z3.mli z3.ml -> z3_stubs.obj z3.{cmi,cmx,obj}
ocamlopt -ccopt "%XCFLAGS% %XINCLUDE%" -cclib %XLIBPATH% -c z3_stubs.c z3.mli z3.ml

REM z3.lib z3.obj z3_stubs.obj -> libz3.lib:
lib /nologo %XLIBPATH% /out:libz3.lib z3.lib z3.obj z3_stubs.obj

REM z3.cmx libz3.lib -> z3.cmxa
ocamlopt -ccopt "%XCFLAGS%" -a -o z3.cmxa -cclib -lz3 z3.cmx


