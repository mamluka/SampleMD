#
# Makefile for DOM examples
#
default: all
all: features data pseudo pseudo_dom text
#
#---------------------------
MK=$(FLIB_ROOT)/fortran.mk
include $(MK)
#---------------------------
#
# Uncomment the following line for debugging support
#
#FFLAGS=$(FFLAGS_DEBUG)
#FFLAGS=$(FFLAGS_CHECK)
#
LIBS=$(LIB_PREFIX)$(LIB_STD) -lflib
#
features: features.o
	$(FC) $(LDFLAGS) -o features features.o  $(LIBS)
data: data.o
	$(FC) $(LDFLAGS) -o data data.o  $(LIBS)
pseudo: m_pseudo_types.o  pseudo.o
	$(FC) $(LDFLAGS) -o pseudo m_pseudo_types.o  pseudo.o  $(LIBS)
pseudo_dom: m_pseudo_types.o m_psdom.o pseudo_dom.o
	$(FC) $(LDFLAGS) -o pseudo_dom m_pseudo_types.o m_psdom.o pseudo_dom.o  $(LIBS)
text:  text.o
	$(FC) $(LDFLAGS) -o text text.o  $(LIBS)
#
clean: 
	rm -f features data pseudo pseudo_dom text  *.o *.$(MOD_EXT)
#










