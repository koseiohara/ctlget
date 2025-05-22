EXE = sample
OBJS = sample_ctlget.o

FC = ifort
#FC = gfortran

ifeq (${FC}, ifort)
	FLAGS = -O2 -warn all -traceback
else
	FLAGS = -O2 -Wall -fbacktrace
endif

IPATH = -I ${HOME}/FortranLib/include
LPATH = -L ${HOME}/FortranLib/lib

%.o : %.f90
	${FC} -c $< ${FLAGS} ${IPATH}

all : ${EXE}

${EXE} : ${OBJS}
	${FC} -o $@ $^ -lctlget ${LPATH}

.PHONY : clean re run

clean :
	rm -fv *.o *.mod ${LIB}

re : clean all

run :
	./${EXE}


