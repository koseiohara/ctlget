LIB = libctlget.a
OBJS = caseconverter.o ctlget.o

EXPORT_OBJ = ctlget.o

FC = ifort

DIR = ${HOME}/FortranLib

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

.PHONY : clean re install uninstall

clean :
	rm -fv *.o *.mod *.a

re : clean all

install :
	cp ${LIB} ${DIR}/lib/
	cp ${EXPORT_OBJ:.o=.mod} ${DIR}/include/

uninstall :
	rm -fv ${DIR}/lib/${LIB}
	rm -fv ${EXPORT_OBJ:%.o=${DIR}/include/%.mod}


