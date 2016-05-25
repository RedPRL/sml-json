default:
	mlton @MLton fixed-heap 0.5g -- -mlb-path-map mlb-path-map -output tests/test tests/main.mlb
	cd tests; ./test
