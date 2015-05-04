run: gason.exe
	./$<
%.exe: %.nim
	nim c --out:$@ $<
