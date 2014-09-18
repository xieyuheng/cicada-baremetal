.ONESHELL:

help:
	@
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   this Makefile helps you to play with               \E[0m "
	echo -e " \E[33;1m     a specific version of cicada language,           \E[0m "
	echo -e " \E[33;1m     which is a porting of the language               \E[0m "
	echo -e " \E[33;1m     to linux operating system.                       \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   dependencies ::                                    \E[0m "
	echo -e " \E[33;1m     fasm                                             \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   build :: compile cicada.fasm                       \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   run :: run cicada                                  \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   clean :: note that, this will also clean up        \E[0m "
	echo -e " \E[33;1m            backup files that ended by '~'            \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   CBR == make clean                                  \E[0m "
	echo -e " \E[33;1m          make build                                  \E[0m "
	echo -e " \E[33;1m          make run                                    \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m     please read the 'Makefile' for more informations \E[0m "
	echo -e " \E[33;1m     happy making ^_^                                 \E[0m "

build:
	@
	fasm play/cicada.fasm play/cicada

run:
	@
	source script/cicada-env.sh &&\
	cicada

CBR:
	@
	echo "	 "        &&\
	make clean        &&\
	echo "	 "        &&\
	make build        &&\
	echo "	 "        &&\
	make run          &&\
	echo "	 "

clean:
	@
	rm -f		       \
	play/cicada	       \
	play/*.bin	       \
	*~ */*~ */*/*~ */*/*/*~	 */*/*/*/*~    &&\
	echo -e "\E[33;1m [ok] clean directory \E[0m"
