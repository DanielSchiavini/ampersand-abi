REM @echo off
cd ..\..\..\ampersand
((ghc -fhpc -with-rtsopts="-M1536m -H256m -K128m" -o runtest.exe -Wall ^
	-isrc/exec:src/lib:dist/build/autogen:src:src/Database/Design/Ampersand/Test:/MinGW/bin ^
	src/Test )^
&& runtest.exe && echo Returned 0 && hpc markup runtest.exe.tix --destdir=../ampersand-abi/Generated/Hpc) ^
|| echo Failure!
pause