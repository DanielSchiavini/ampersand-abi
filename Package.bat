@REM First the documentation

SET BOX=..\..\..\Dropbox\OU\39~42 Afstudeerproject\Ampersand

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
copy "%BOX%\Parsing\Parsing v1.0.pdf" Package\Delivered\
copy "%BOX%\Planning\Planning v2.*.pdf" Package\Delivered\
copy "%BOX%\fase 3b - Onderzoekscontext\ResearchContext v1.0.pdf" Package\Delivered\
copy "%BOX%\Ampersand Methodology\AmpersandApproach v1.0.pdf" Package\Delivered\

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
xcopy /S ..\ampersand\src\Database\Design\Ampersand\Input\ADL1\Pars*.hs Package\Code\Input\
xcopy /S ..\ampersand\src\Database\Design\Ampersand\Input\ADL1\Lex*.hs Package\Code\Input\
xcopy /S ..\ampersand\src\Database\Design\Ampersand\Test\*.hs Package\Code\Test\
xcopy /S ..\ampersand\src\Database\Design\Ampersand\Core\Parse*.hs Package\Code\Core\
xcopy /S ..\ampersand\src\Database\Design\Ampersand\ADL1\Pretty*.hs Package\Code\Core\
xcopy "%BOX%\Time tracking ABI.xlsx" Package\Documentation\

@REM  Packaging it all
cd Package
rm Package.zip
..\..\..\..\Dropbox\Software\7za a ..\Package.zip *
cd ..
rm Package -r
