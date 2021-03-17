SCRIPT=favgames.scm

build:
	rh1 install
	npm install

run: $(TARGET) $(RHEINGAU)
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) nodemon -e scm --ignore gosh-modules/ --exec violet $(SCRIPT)

## docker run --rm -p 2222:2222 -v$PWD:/code -w /code -t -i gauche-violet_gosh make debug

clean:
	rm -rf *~ *.o $(TARGET) gosh-modules $(RHEINGAU) $(TARGET).dSYM
