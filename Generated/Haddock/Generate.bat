@cd ..\..\..\ampersand
@cabal haddock --all
@xcopy dist\doc\html\ampersand\* ..\ampersand-abi\Generated\Haddock\html\ /Y /S
@REM haddock Main --verbosity=3 --optghc="-i../dist/build/autogen" --html -o ../../ampersand-abi/Generated/Haddock/html/
pause
