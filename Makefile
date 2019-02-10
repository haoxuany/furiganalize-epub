
mlton:
	mkdir -p bin/
	mlton -output bin/furiganalize sources.mlb

smlnj:
	sml -m sources.cm

clean:
	rm -rf bin/
