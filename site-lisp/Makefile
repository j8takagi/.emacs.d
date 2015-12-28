SITELISPS := $(shell sh -c "ls -F | grep /")

.PHONY: all install clean distclean

all:
	$(foreach dir,$(SITELISPS),$(MAKE) -sC $(dir) all; )

install: all
	$(foreach dir,$(SITELISPS),$(MAKE) -sC $(dir) install; )

clean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) clean; )

distclean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) distclean; )
