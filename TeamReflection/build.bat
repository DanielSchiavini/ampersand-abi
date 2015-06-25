@echo off

:: Run Cleanup  
call:cleanup

:: Run pdflatex -&gt; bibtex -&gt; pdflatex -&gt; pdflatex  
echo Running pdflatex for TeamReflection...
pdflatex TeamReflection>nul
pdflatex TeamReflection>nul
echo Glossary...
makeglossaries TeamReflection
pdflatex TeamReflection>nul
find "Warning" TeamReflection.log
where bibtex>nul         || echo Error: Could not find bibtex.exe in the PATH
pause

:: Run Cleanup
call:cleanup

:: Cleanup Function
:cleanup  
:: del /q *.dvi
:: del /q *.out
:: del /q *.log
:: del /q *.aux
:: del /q *.bbl
:: del /q *.blg
:: del /q *.brf
del /q *.toc

goto:eof
