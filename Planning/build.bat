@echo off

:: Run Cleanup  
call:cleanup

:: Run pdflatex -&gt; bibtex -&gt; pdflatex -&gt; pdflatex  
pdflatex Planning
:: bibtex  Planning
makeglossaries Planning
pdflatex Planning
pdflatex Planning
:: (cat Planning.log | grep -i Warn)

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