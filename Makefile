######
#
# A Simple Makefile
#
######
MAKEFLAGS += --silent

######

PATSCC=\
$(PATSHOME)/bin/patscc
PATSOPT=\
$(PATSHOME)/bin/patsopt
ATSCC2JS=\
$(PATSHOME)/bin/atscc2js

######

DIR=./DATS

######

all::

######
#

all:: ; make -C $(DIR) && printf "$?" && echo "Build Successful" \
|| printf "\nBuilding tacc Failed\n" 
all:: ; [ -f "./DATS/tacc" ] && mv ./DATS/tacc .

#
######

regress:: \
tacc; ./$<

######

testall:: all
testall:: regress
testall:: cleanall

######

test0:: cleanall
test0:: all

# trytest: test0
# trytest: ; ./tacc tcatsc ${ARG1}

######

install:: ; [ -f "./tacc" ] && \
echo "Installing tacc at \$$TEMPTORY/bin/" && mv tacc $(TEMPTORY)/bin/ || \
echo "Oops. Something went wrong. Build tacc manually with the command 'make && make install'"


######

# check_install:: ; printf "acc is located at: " && which acc

######

testrun:: ; @printf "\e[33mbuilding...\e[0m\n"
testrun:: test0
testrun:: ; @printf "\e[36mrunning tests... \e[0m\n" && printf "\e[36m>>>\e[0m\n"
testrun:: ; @printf "Each test will output the current error message (with patscc) and the corresponding pretty-printed message (with acc)\n"
testrun:: ; -@sh ./TEST/tests.sh

######

# run-withargs:: ; -@./tacc -tcats ${ARG1}
# run-valgrind:: ; -@valgrind ./acc -tcats ${ARG1} >/dev/null

######

clean:: ; make -C $(DIR) clean
clean:: ; rm -f .log
clean:: ; rm -f *~
clean:: ; rm -f *_?ats.c

######

cleanall:: clean
cleanall:: ; make -C $(DIR) cleanall
cleanall:: ; rm -f tacc
###### end of [Makefile] ######
