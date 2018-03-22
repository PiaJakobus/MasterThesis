FC = gfortran
FCFLAGS = -O3
OBJECTS = main.o ode.o eos_tpp.o rk.o parameters.o

%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

release: $(OBJECTS)
	$(FC) -o main $(FCFLAGS) $(OBJECTS)
clean: 
	rm -f *.o tabelle_rel.csv p_tabelle.csv fit.csv
