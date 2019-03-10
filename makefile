#EXTRA+=-d:nativeStacktrace
EXTRA+=-d:debug
#EXTRA+=--debugger:native
#EXTRA+=--debugger:endb
default: run
#lib: libnjson.so
#libnjson.so: gason.nim
#	nim c ${EXTRA} --app:lib --out:$@ $<
run: gason.exe
	./$<
%.exe: %.nim
	nim c ${EXTRA} --out:$@ $<
clean:
	rm -f *.exe lib*.so
