.ONESHELL:

help:
	@
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   dependencies                                            \E[0m "
	echo -e " \E[33;1m     fasm   to compile                                     \E[0m "
	echo -e " \E[33;1m     qemu   to test x86-64 kernel image                    \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   Makefile functions                                      \E[0m "
	echo -e " \E[33;1m     linux                                                 \E[0m "
	echo -e " \E[33;1m     x86-64-kernel                                         \E[0m "
	echo -e " \E[33;1m     qemu-x86-64-kernel			             \E[0m "
	echo -e " \E[33;1m     clean                                                 \E[0m "
	echo -e " \E[33;1m							     \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "
	echo -e " \E[33;1m   please read the Makefile for more informations          \E[0m "
	echo -e " \E[33;1m							     \E[0m "
	echo -e " \E[33;1m   I wish you happy making ^_^                             \E[0m "
	echo -e " \E[33;1m                                                           \E[0m "

all:
	@
	echo "	 "                                    &&\
	make linux                                    &&\
	echo "	 "			              &&\
	make x86-64-kernel                            &&\
	echo "	 "

linux:
	cd vm/x86-64/linux/                           &&\
	fasm -m 500000 cicada.fasm cicada

x86-64-kernel:
	@
	echo "	 "                                    &&\
	make compile-bootloader-for-x86-64-kernel     &&\
	echo "	 "			              &&\
	make compile-x86-64-kernel                    &&\
	echo "	 "			              &&\
	make burn-cicada-image-for-x86-64-kernel      &&\
	echo "	 "

compile-bootloader-for-x86-64-kernel:
	cd vm/x86-64/baremetal/                       &&\
	fasm bootloader.fasm bootloader.bin

compile-x86-64-kernel:
	cd vm/x86-64/baremetal/                       &&\
	fasm -m 500000 cicada-kernel.fasm cicada-kernel.bin

burn-cicada-image-for-x86-64-kernel:
	cd vm/x86-64/baremetal/                       &&\
	fasm cicada-image.fasm

qemu-x86-64-kernel:
	cd vm/x86-64/baremetal/                       &&\
	qemu-system-x86_64			        \
	  -enable-kvm				        \
	  -vga std				        \
	  -m 1G 				        \
	  -fda cicada-image.bin

dangerous-burn-sdb-with-cicada-image-x86-64:
	make x86-64-kernel                            &&\
	cd vm/x86-64/baremetal/                       &&\
	sudo dd if=cicada-image.bin of=/dev/sdb bs=2M

vmdk-x86-64-kernel:
	cd vm/x86-64/baremetal/                       &&\
	qemu-img convert -O vmdk cicada-image.bin cicada-image.vmdk

clean*~:
	rm -f *~ */*~ */*/*~ */*/*/*~ */*/*/*/*~  */*/*/*/*/*~  

clean*.bin:
	rm -f *.bin */*.bin */*/*.bin */*/*/*.bin */*/*/*/*.bin  */*/*/*/*/*.bin

clean:
	make clean*~                                  &&\
	make clean*.bin                               &&\
	rm -f vm/x86-64/linux/cicada                  &&\
	rm -f vm/x86-64/baremetal/*.vmdk              &&\
	echo -e "\E[33;1m [ok] clean directory \E[0m"
