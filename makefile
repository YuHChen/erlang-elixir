classDemo: classDemo.c
	gcc -std=c99 -g classDemo.c -o classDemo

demo: classDemo
	./classDemo

clean:
	rm -f classDemo *~ *#