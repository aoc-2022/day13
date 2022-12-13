(cat /tmp/aoc/input;echo "2]]\n6]]") | sed 's/\[//g;s|\]|%|g' | sort -n | grep -v "^$" | grep -n "^[26]%%$" | sed "s/:2.*/\*/;s|:.*||" | xargs echo | bc
