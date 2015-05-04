lib: libnjson.so
libnjson.so: gason.nim
	nim c --app:lib --out:$@ $<
run: gason.exe
	./$<
%.exe: %.nim
	nim c --out:$@ $<
