mkdir Documentation

cd Haddock
@REM call Generate.bat
cd ..
xcopy /S Haddock\html Documentation\Haddock\

xcopy /S HLint\*.html   Documentation\HLint\
xcopy /S Hpc\*.html     Documentation\Hpc\
xcopy /S Diagrams\*.pdf Documentation\Diagrams\
xcopy /S Diagrams\*.png Documentation\Diagrams\

mkdir Documentation\Errors
cd Errors
call build.bat
cd ..
copy Errors\Errors.pdf Documentation\Errors

mkdir Documentation\Ebnf
copy Ebnf\index.html Documentation\Ebnf\
copy Ebnf\ADL.ebnf Documentation\Ebnf\
xcopy Ebnf\diagram Documentation\Ebnf\diagram\

rm Documentation.zip
..\..\..\..\Dropbox\Software\7za a Documentation.zip Documentation\*
rm Documentation -r
