all: debug

debug: src/SDL.hs src/Main.hs
	ghc $^ -debug -rtsopts -threaded -dynamic -lSDL2 -o bin/main
	$(RM) src/Main.hi src/Main.o src/SDL.hi src/SDL.o

release: src/SDL.hs src/Main.hs
	ghc $^ -O2 -rtsopts=none -threaded -dynamic -lSDL2 -o bin/main
	strip -p --strip-unneeded --remove-section=.comment ./bin/main
	$(RM) src/Main.hi src/Main.o src/SDL.hi src/SDL.o

windows: src\SDL.hs src\Main.hs
	ghc  $^ -O2 -rtsopts=none -threaded -L'C:\msys64\mingw64\lib' -lSDL2 -optl-mwindows -o bin\main
	strip -p --strip-unneeded --remove-section=.comment .\bin\main.exe

clean:
	$(RM) src/Main.hi \
              src/Main.o  \
              src/SDL.hi  \
              src/SDL.o   \
              bin/main
