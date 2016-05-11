
FC=ifort

intel:
		( $(MAKE) all \
        "FC = ifort")
		
gnu:
		( $(MAKE) all \
        "FC = gfortran")
		
pgi: 
		( $(MAKE) all \
		"FC = pgfortran")





all: list.o node.o key.o main.f90 list.f90 node.f90
	$(FC) main.f90 *.o

list.o: node.o list.f90 node.f90
	$(FC) -c list.f90 node.o

node.o: key.o node.f90
	$(FC) -c node.f90 

key.o: key.f90
	$(FC) -c key.f90

clean:
	rm a.out *.o *.mod
	
