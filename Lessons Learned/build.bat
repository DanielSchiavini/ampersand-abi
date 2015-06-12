@echo off

:: Run Cleanup  
call:cleanup

:: Run pdflatex -&gt; bibtex -&gt; pdflatex -&gt; pdflatex  
echo Running pdflatex for Lessons_Learned...
pdflatex Lessons_Learned>nul
echo Running bibtex...
bibtex  ac
bibtex  nac
bibtex  pr
pdflatex Lessons_Learned>nul
pdflatex Lessons_Learned>nul
echo Glossary...
makeglossaries Lessons_Learned
pdflatex Lessons_Learned>nul
find "Warning" Thesis.log
where bibtex>nul         || echo Error: Could not find bibtex.exe in the PATH
where makeglossaries>nul || echo Error: Could not find makeglossaries.exe in the PATH
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
