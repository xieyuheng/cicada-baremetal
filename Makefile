.ONESHELL:

help:
	@
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   this Makefile helps you to play with               \E[0m "
	echo -e " \E[33;1m     a specific version of cicada language,           \E[0m "
	echo -e " \E[33;1m     which is a simple operating system of x86_64.    \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   dependencies ::                                    \E[0m "
	echo -e " \E[33;1m     fasm, qemu                                       \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   build == make compile-bootloader                   \E[0m "
	echo -e " \E[33;1m            make compile-cicada-kernel                \E[0m "
	echo -e " \E[33;1m            make burn-cicada.image                    \E[0m "
	echo -e " \E[33;1m            make write-programs-to-cicada.image       \E[0m "
	echo -e " \E[33;1m                                                      \E[0m "
	echo -e " \E[33;1m   run :: use qemu (the 'Quick EMUlator')             \E[0m "
	echo -e " \E[33;1m          to emulate the OS                           \E[0m "
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
	make build-cicada
run:
	make run-cicada
CBR:
	@
	echo "	 "        &&\
	make clean        &&\
	echo "	 "        &&\
	make build        &&\
	echo "	 "        &&\
	make run          &&\
	echo "	 "

compile-bootloader:
	@ 
	cd play
	fasm -m 500000 bootloader.fasm	 bootloader.bin 

compile-cicada-kernel:
	@ 
	cd play                                   
	fasm -m 500000 cicada-kernel.fasm cicada-kernel.bin

burn-cicada.image:
	@ 
	cd play
	fasm cicada.fasm

write-programs-to-cicada.image:
	@ 
	cd play

build-cicada:
	@
	echo "	 "                               &&\
	make compile-bootloader		         &&\
	echo "	 "			         &&\
	make compile-cicada-kernel	         &&\
	echo "	 "			         &&\
	make burn-cicada.image		         &&\
	echo "	 "			         &&\
	make write-programs-to-cicada.image	 &&\
	echo "	 "

run-cicada:
	@ 
	cd play
	qemu-system-x86_64			   \
	  -enable-kvm				   \
	  -vga std				   \
	  -m 1G 				   \
	  -fda cicada.image

dangerous-burn-sdb-with-cicada.image:
	@ 
	make build-cicada                            &&\
	cd play                                      &&\
	sudo dd if=cicada.image of=/dev/sdb bs=2M        			  

CBR-cicada:
	@
	echo "	 "         &&\
	make clean         &&\
	echo "	 "         &&\
	make build-cicada  &&\
	echo "	 "         &&\
	make run-cicada    &&\
	echo "	 "

virtualbox-vmdk:
	@
	cd play
	qemu-img convert -O vmdk cicada.image cicada.vmdk

clean:
	@
	rm -f		       \
	play/*.bin	       \
	play/*.image	       \
	play/*.vmdk	       \
	*~ */*~ */*/*~ */*/*/*~	 */*/*/*/*~    &&\
	echo -e "\E[33;1m [ok] clean directory \E[0m"
