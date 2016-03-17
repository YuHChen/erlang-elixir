# To build modules outside of the kernel tree, we run "make"
# in the kernel source tree; the Makefile there then includes this
# Makefile once again.

# This conditional selects whether we are being included from the
# kernel Makefile or not.
ifeq ($(KERNELRELEASE),)

	# Assume the source tree is where the running kernel was built
	# You should set KERNELDIR in the environment if it's elsewhere
	KERNELDIR ?= /lib/modules/$(shell uname -r)/build

	# The current directory is passed to sub-makes as argument
	PWD := $(shell pwd)

modules:
	@echo "compiling modules..."
	@$(MAKE) -C $(KERNELDIR) M=$(PWD) modules
	@echo "compiling user.c into executable user..."
	@gcc -o user user.c
	@echo "done compiling."

modules_install:
	@echo "installing modules..."
	$(MAKE) -C $(KERNELDIR) M=$(PWD) modules_install
	@echo "done installing modules."

clean:
	@echo "cleaning..."
	@rm -rf *.o *~ core .depend .*.cmd *.ko *.mod.c .tmp_versions Module.symvers modules.order user *#
	@echo "all clean!"

again: clean
	@make

compress: clean
	tar -cvf ../astrong3_assignment5.tar ../astrong3_assignment5
	gzip ../astrong3_assignment5.tar

.PHONY: modules modules_install clean
	#@echo ".phony"

else
	# called from kernel build system: just declare what our modules are
	obj-m := pipe_module.o
endif
