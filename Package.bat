cd Generated\Haddock
@REM call Generate.bat
cd ..\..
xcopy /S Generated\Haddock\html Package\Documentation\Haddock\

xcopy /S Generated\HLint\*.html   Package\Documentation\HLint\
xcopy /S Generated\Hpc\*.html     Package\Documentation\Hpc\
xcopy /S Generated\Diagrams\*.pdf Package\Documentation\Diagrams\
xcopy /S Generated\Diagrams\*.png Package\Documentation\Diagrams\

mkdir Package\Documentation\Errors
cd Generated\Errors
call build.bat
cd ..\..
copy Generated\Errors\Errors.pdf Package\Documentation\Errors
xcopy /S Generated\Errors\Scripts\*.adl Package\Documentation\Errors\Scripts\

mkdir Package\Documentation\Ebnf
copy Generated\Ebnf\index.html Package\Documentation\Ebnf\
copy Generated\Ebnf\ADL.ebnf Package\Documentation\Ebnf\
xcopy Generated\Ebnf\diagram Package\Documentation\Ebnf\diagram\

cd Package
rm Package.zip
..\..\..\..\Dropbox\Software\7za a ..\Package.zip *
cd ..
rm Package -r
