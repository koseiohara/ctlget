LIB = libctlget.a
OBJS = caseconverter.o ctlinfo.o

FC = ifort
#FC = gfortran
ifeq (${FC}, ifort)
	FLAGS = -O3 -warn all -traceback
else
	FLAGS = -O3 -Wall -fbacktrace
endif

%.o : %.f90
	${FC} -c $< ${FLAGS}

all : ${LIB}

${LIB} : ${OBJS}
	ar rcs $@ $^

.PHONY : clean re

clean :
	rm -fv *.o *.mod *.a

re : clean all


