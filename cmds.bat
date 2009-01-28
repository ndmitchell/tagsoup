@echo off
if "%1"=="" goto none
goto %1


:benchmark
ghc --make Main -odir obj\o -hidir obj\o -O -o tagsoup_o.exe
tagsoup_o.exe time
tagsoup_o.exe timefile ketil.xml
goto end


:profile
ghc --make Main -odir obj\p -hidir obj\p -O -o -prof -auto-all tagsoup_p.exe
del tagsoup_p_*.prof
tagsoup_p.exe time +RTS -p
ren tagsoup_p.prof tagsoup_p_time.prof
tagsoup_p.exe timefile ketil.xml +RTS -p
ren tagsoup_p.prof tagsoup_p_ketil.prof
goto end


:dump
ghc --make Main -odir obj\o -hidir obj\o -O -o tagsoup_o.exe
call %0 core Text\StringLike.hs stringlike
call %0 core Text\HTML\TagSoup\Parser2.hs parser
call %0 core Text\ParserCombinators\LazyParse.hs lazyparse
goto end

:core
shift
copy %1 obj\o\%1
pushd obj\o
ghc %1 -ddump-simpl -c > ..\..\%2.core
popd
goto end


:none
echo Enter command, try one of:
echo benchmark, profile, dump
goto end


:end
