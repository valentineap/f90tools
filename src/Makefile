OBJ=../obj
MOD=../mod
LIB=../lib
BIN=..

F95=gfortran
FLAGS=-J$(MOD) -Ofast -fPIC
F95COMPILE=gfortran -c $(FLAGS) -Wall
F95LINK=gfortran $(FLAGS)

.PHONY: all mod_log mod_types mod_ndk mod_foptparse

all: lib demo_tools

lib: $(LIB)/libf90tools.a

mod_types: mod_log $(OBJ)/mod_types.o
mod_log: $(OBJ)/mod_log.o
mod_ndk: mod_types mod_log $(OBJ)/mod_ndk.o
mod_foptparse: mod_log $(OBJ)/mod_foptparse.o
demo_tools: mod_log mod_types mod_foptparse mod_ndk $(BIN)/demo_tools

$(BIN)/demo_tools: $(OBJ)/demo_tools.o lib
	$(F95LINK) -o $@ $< -L$(LIB) -lf90tools

$(OBJ)/demo_tools.o: demo_tools.f90
	$(F95COMPILE) -o $@ $+

$(OBJ)/mod_types.o: mod_types.f90
	$(F95COMPILE) -o $@ $+

$(OBJ)/mod_log.o: mod_log.f90
	$(F95COMPILE) -o $@ $+

$(OBJ)/mod_ndk.o: mod_ndk.f90
	$(F95COMPILE) -o $@ $+

$(OBJ)/mod_foptparse.o: mod_foptparse.f90
	$(F95COMPILE) -o $@ $+

$(LIB)/libf90tools.a: \
		$(OBJ)/mod_log.o \
		$(OBJ)/mod_types.o \
		$(OBJ)/mod_foptparse.o \
		$(OBJ)/mod_ndk.o
	ar -rcs $@ $+
