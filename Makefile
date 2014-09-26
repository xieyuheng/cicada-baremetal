.ONESHELL:

help:
	@
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   dependencies ::                                         \E[0m "
	echo -e " \E[33;1m     fasm : to compile                                     \E[0m "
	echo -e " \E[33;1m     qemu : to test x86-64 kernel image                    \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   Makefile functions ::                                   \E[0m "
	echo -e " \E[33;1m     build-cicada-for-linux                                \E[0m "
	echo -e " \E[33;1m     build-cicada-kernel-for-x86-64                        \E[0m "
	echo -e " \E[33;1m     test-cicada-kernel-for-x86-64			     \E[0m "
	echo -e " \E[33;1m     clean                                                 \E[0m "
	echo -e " \E[33;1m							     \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   please read the 'Makefile' for more informations        \E[0m "
	echo -e " \E[33;1m   happy making ^_^                                        \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "

build-cicada-for-linux:
	@
	cd play
	fasm -m 500000 cicada.fasm cicada

compile-bootloader-for-x86-64:
	@ 
	cd play
	fasm -m 500000 bootloader.fasm	 bootloader.bin 

compile-cicada-kernel-for-x86-64:
	@ 
	cd play                                   
	fasm -m 500000 cicada-kernel.fasm cicada-kernel.bin

burn-cicada-image-for-x86-64:
	@ 
	cd play
	fasm cicada-image.fasm

build-cicada-kernel-for-x86-64:
	@
	echo "	 "                               &&\
	make compile-bootloader-for-x86-64       &&\
	echo "	 "			         &&\
	make compile-cicada-kernel-for-x86-64    &&\
	echo "	 "			         &&\
	make burn-cicada-image-for-x86-64        &&\
	echo "	 "

test-cicada-kernel-for-x86-64:
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
