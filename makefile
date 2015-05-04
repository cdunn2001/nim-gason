lib: libnjson.so
libnjson.so: gason.nim
	nim c -d:release --app:lib --out:$@ $<
run: gason.exe
	./$<
%.exe: %.nim
	nim c --out:$@ $<
