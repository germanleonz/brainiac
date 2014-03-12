main: clean alex happy
	ghc -O2 Main.hs

alex:
	alex -g LexBrainiac.x

happy:
	happy -g -s SinBrainiac.y

run:
	./Sapphire $(file)

ghci:
	ghci Sapphire

file:
	./Sapphire ../ejemplos/sapphire/ejemplo.sp

clean:
	@rm *.hi *.o || true
	@rm LexBrainiac.hs || true
	@rm SinBrainiac.hs || true
	@rm Main || true
