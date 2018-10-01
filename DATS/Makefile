######
#
# A Simple Makefile
#
######
MAKEFLAGS += --silent

######

PATSCC=acc -pc
# \
# $(PATSHOME)/bin/patscc
PATSOPT=\
$(PATSHOME)/bin/patsopt
ATSCC2JS=\
$(PATSHOME)/bin/atscc2js

######

all::

######
#

all:: \
acc
acc: \
main.dats; \
$(PATSCC) -cleanaft -D_GNU_SOURCE -O2 -DATS_MEMALLOC_LIBC -o $@ $< -latslib
# $(PATSCC) -cleanaft -O3 -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -o $@ $< -latslib
#
######

regress:: \
acc; ./$<

######

testall:: all
testall:: regress
testall:: cleanall

######

test2:: cleanall
test2:: all
test2:: ; (time ./acc -pc 806_errors.dats)

######

test0:: cleanall
test0:: all

######

runit:: ; -@./acc -tcats ${ARG1} 

######

run:: ; @echo -e "\e[33mbuilding...\e[0m"
run:: test0
run:: ; @echo -e "\e[36mrunning acc\e[0m" && echo -e "\e[36m>>>\e[0m"
run:: runit

######

runitt:: ; -@valgrind ./acc -tcats ${ARG1} ${ARG2} >/dev/null

######

runt:: ; @echo -e "\e[33mbuilding...\e[0m"
runt:: test0
runt:: ; @echo -e "\e[36mrunning acc\e[0m" && echo -e "\e[36m>>>\e[0m"
runt:: runitt


######

clean:: ; rm -f *~
clean:: ; rm -f .log
clean:: ; rm -f *_?ats.c

######

cleanall:: clean
cleanall:: ; rm -f acc
###### end of [Makefile] ######
