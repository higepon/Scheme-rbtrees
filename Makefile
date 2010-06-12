check:
	rm -f rb-trees.dot
	nmosh -p test.ss
	dot -Tpng rb-trees.dot -o rb-trees.png
	eog rb-trees.png
