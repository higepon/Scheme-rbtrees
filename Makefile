check:
	rm -f rb-trees.dot
	nmosh test.ss
	dot -Tpng rbtree.dot -o rb-trees.png
	eog rb-trees.png
