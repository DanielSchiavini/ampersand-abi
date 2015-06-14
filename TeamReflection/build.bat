@echo off

:: Run Cleanup  
call:cleanup

:: Run pdflatex -&gt; bibtex -&gt; pdflatex -&gt; pdflatex  
echo Running pdflatex for LessonsLearned...
pdflatex LessonsLearned>nul
echo Running bibtex...
bibtex  ac
bibtex  nac
bibtex  pr
pdflatex LessonsLearned>nul
pdflatex LessonsLearned>nul
echo Glossary...
makeglossaries LessonsLearned
pdflatex LessonsLearned>nul
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
