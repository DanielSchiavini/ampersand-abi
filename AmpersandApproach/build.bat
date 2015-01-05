@echo off

:: Run Cleanup  
call:cleanup

:: Run pdflatex -&gt; bibtex -&gt; pdflatex -&gt; pdflatex  
pdflatex AmpersandApproach
pdflatex AmpersandApproach
:: bibtex  AmpersandApproach
makeglossaries AmpersandApproach
pdflatex AmpersandApproach
pdflatex AmpersandApproach
@cls && (cat AmpersandApproach.log | grep -i Warn)
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
