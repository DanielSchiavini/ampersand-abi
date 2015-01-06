@echo off

findstr /r "\-\-\-.*" *.hs > ADL-generated.temp
call BatchSubstitute "---" "" ADL-generated.temp > ADL-generated.ebnf

call BatchSubstitute "Parser.hs:"     "" ADL-generated.ebnf > ADL-generated.temp
call BatchSubstitute "UU_Scanner.hs:" "" ADL-generated.temp > ADL-generated.ebnf

del ADL-generated.temp

echo Syntax generated to ADL-generated.ebnf