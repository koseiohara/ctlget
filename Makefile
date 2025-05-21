EXE = exe

OBJS = caseconverter.o ctlinfo.o test.o

FC = ifort
FLAGS = -O2 -warn all -traceback -check all

%.o : %.f90
	${FC} -c $< ${FLAGS}

all : ${EXE}

${EXE} : ${OBJS}
	${FC} -o $@ $^

.PHONY : clean re

clean :
	rm -fv *.o *.mod ${EXE}

re : clean all

run :
	./${EXE}

