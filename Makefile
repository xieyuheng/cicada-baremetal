.ONESHELL:

help:
	@
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   dependencies ::                                         \E[0m "
	echo -e " \E[33;1m     fasm, qemu                                            \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   build-cicada-for-linux :: compile an ELF file           \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   build-cicada-kernel :: build the x86-64 OS kernel       \E[0m "
	echo -e " \E[33;1m     == make compile-bootloader		             \E[0m "
	echo -e " \E[33;1m	  make compile-cicada-kernel		             \E[0m "
	echo -e " \E[33;1m	  make burn-cicada-image		             \E[0m "
	echo -e " \E[33;1m	  make write-programs-to-cicada-image	             \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   run-cicada-kernel :: use QEMU to test OS kernel         \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   clean :: note that, this will also clean up             \E[0m "
	echo -e " \E[33;1m            backup files that ended by '~'                 \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m     please read the 'Makefile' for more informations      \E[0m "
	echo -e " \E[33;1m     happy making ^_^                                      \E[0m "

build-cicada-for-linux:
	@
	cd play
	fasm -m 500000 cicada.fasm cicada

compile-bootloader:
	@ 
	cd play
	fasm -m 500000 bootloader.fasm	 bootloader.bin 

compile-cicada-kernel:
	@ 
	cd play                                   
	fasm -m 500000 cicada-kernel.fasm cicada-kernel.bin

burn-cicada-image:
	@ 
	cd play
	fasm cicada-image.fasm

write-programs-to-cicada-image:
	@ 
	cd play

build-cicada-kernel:
	@
	echo "	 "                               &&\
	make compile-bootloader		         &&\
	echo "	 "			         &&\
	make compile-cicada-kernel	         &&\
	echo "	 "			         &&\
	make burn-cicada-image		         &&\
	echo "	 "			         &&\
	make write-programs-to-cicada-image	 &&\
	echo "	 "

run-cicada-kernel:
	@ 
	cd play
	qemu-system-x86_64			   \
	  -enable-kvm				   \
	  -vga std				   \
	  -m 1G 				   \
	  -fda cicada-image.bin

dangerous-burn-sdb-with-cicada-image:
	@ 
	make build-cicada-kernel                            &&\
	cd play                                      &&\
	sudo dd if=cicada-image.bin of=/dev/sdb bs=2M        			  

virtualbox-vmdk:
	@
	cd play
	qemu-img convert -O vmdk cicada-image.bin cicada-image.vmdk

clean:
	@
	rm -f		       \
	play/cicada            \
	play/*.bin	       \
	play/*.vmdk	       \
	*~ */*~ */*/*~ */*/*/*~	 */*/*/*/*~    &&\
	echo -e "\E[33;1m [ok] clean directory \E[0m"
