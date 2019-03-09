
mlton:
	mkdir -p bin/
	mlton -output bin/furiganalize sources.mlb

smlnj:
	mkdir -p bin/
	sml njbuild.sml

clean:
	rm -rf bin/
