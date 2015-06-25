@REM First the documentation

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
@REM call build.bat
cd ..\..
copy Generated\Errors\Errors.pdf Package\Documentation\Errors
xcopy /S Generated\Errors\Scripts\*.adl Package\Documentation\Errors\Scripts\

mkdir Package\Documentation\Ebnf
copy Generated\Ebnf\index.html Package\Documentation\Ebnf\
copy Generated\Ebnf\ADL.ebnf Package\Documentation\Ebnf\
xcopy Generated\Ebnf\diagram Package\Documentation\Ebnf\diagram\
xcopy "Generated\Ampersand for Notepad++.xml" Package\Documentation\

@REM Other documents
mkdir Package\Delivered
copy Parsing\Parsing.pdf Package\Delivered\
copy Planning\Planning.pdf Package\Delivered\
copy ResearchContext\ResearchContext.pdf Package\Delivered\
copy AmpersandApproach\AmpersandApproach.pdf Package\Delivered\

@REM Main documents
cd Thesis
call build.bat
cd ..
copy Thesis\Thesis.pdf Package\
cd TeamReflection
call build.bat
cd ..
copy TeamReflection\TeamReflection.pdf Package\

@REM Code and timesheet
xcopy /S ..\ampersand\src\Database\Design\Ampersand\Input\ADL1\Pars*.hs Package\Code\
xcopy /S ..\ampersand\src\Database\Design\Ampersand\Input\ADL1\Lex*.hs Package\Code\
xcopy "..\..\..\Dropbox\OU\39~42 Afstudeerproject\Ampersand\Time tracking ABI.xlsx" Package\Documentation\

@REM  Packaging it all
cd Package
rm Package.zip
..\..\..\..\Dropbox\Software\7za a ..\Package.zip *
cd ..
rm Package -r
