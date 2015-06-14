@echo off

cd List
runhaskell GenErrorList
cd ..

pdflatex Errors
pdflatex Errors>nul
pdflatex Errors>nul
find "Warning" Errors.log

goto:eof
