@echo off

copy ..\..\ampersand\src\Database\Design\Ampersand\Input\ADL1\*.hs .
copy ..\..\ampersand\src\Database\Design\Ampersand\Core\ParseTree.hs .

findstr /r "\-\-\-.*" *.hs > ADL-generated.temp

call BatchSubstitute "Parser.hs: "      "" ADL-generated.ebnf > ADL-generated.temp
call BatchSubstitute "ParsingLib.hs: "  "" ADL-generated.temp > ADL-generated.ebnf
call BatchSubstitute "ParsingLib.hs:--" "" ADL-generated.ebnf > ADL-generated.temp
call BatchSubstitute "Lexer.hs:--"      "" ADL-generated.temp > ADL-generated.ebnf

call BatchSubstitute "---" "" ADL-generated.ebnf > ADL-generated.temp

del ADL-generated.ebnf
mv ADL-generated.temp ADL-generated.ebnf

echo Syntax generated to ADL-generated.ebnf
pause