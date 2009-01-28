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
tagsoup_p.exe time +RTS -p
ren tagsoup_p.prof tagsoup_p_time.prof
tagsoup_p.exe timefile ketil.xml +RTS -p
ren tagsoup_p.prof tagsoup_p_ketil.prof
goto end


:dump
echo dump here
goto end


:none
echo Enter command, try one of:
echo benchmark, profile, dump
goto end


:end
