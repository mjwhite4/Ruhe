
../ZobristM.f90: generate_zobrist_keys
	./generate_zobrist_keys
	mv ZobristM.f90 ..

generate_zobrist_keys: RandomUtilsM.f90 generate_zobrist_keys.f90
	gfortran -o generate_zobrist_keys \
		RandomUtilsM.f90 generate_zobrist_keys.f90

clean:
	rm -f ./generate_zobrist_keys *.mod

distclean: clean
	rm -f ZobristM.f90

