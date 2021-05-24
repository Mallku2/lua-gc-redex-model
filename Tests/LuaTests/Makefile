define test-file
	echo '(require "$(1).rkt") (test-$(1))' | racket -i
endef

define test-file-parts
	echo '(require "$(1).rkt") $(2)' | racket -i
endef

all: calls closure constructs errors events locals math nextvar sort strings vararg

with-gc: gc all

calls:
	$(call test-file-parts,calls,(test-calls_1a)(test-calls_1b)(test-calls_1c)(test-calls_2a)(test-calls_2b)(test-calls_3a)(test-calls_3b)(test-calls_3c)(test-calls_4)(test-calls_5)(test-calls_6))

# to execute just the modules from calls and obtain parallelism
only_calls: calls_1a calls_1b calls_1c calls_2a calls_2b calls_3a calls_3b calls_3c calls_4 calls_5 calls_6

calls_1a:
	$(call test-file-parts,calls,(test-calls_1a))

calls_1b:
	$(call test-file-parts,calls,(test-calls_1b))

calls_1c:
	$(call test-file-parts,calls,(test-calls_1c))

calls_2a:
	$(call test-file-parts,calls,(test-calls_2a))

calls_2b:
	$(call test-file-parts,calls,(test-calls_2b))

calls_3a:
	$(call test-file-parts,calls,(test-calls_3a))

calls_3b:
	$(call test-file-parts,calls,(test-calls_3b))

calls_3c:
	$(call test-file-parts,calls,(test-calls_3c))

calls_4:
	$(call test-file-parts,calls,(test-calls_4))

calls_5:
	$(call test-file-parts,calls,(test-calls_5))

calls_6:
	$(call test-file-parts,calls,(test-calls_6))

##########################################################
closure:
	$(call test-file-parts,closure,(test-closure_1)(test-closure_2)(test-closure_3))

# just the modules from closure
only_closure: closure_1 closure_2 closure_3

closure_1:
	$(call test-file-parts,closure,(test-closure_1))

closure_2:
	$(call test-file-parts,closure,(test-closure_2))

closure_3:
	$(call test-file-parts,closure,(test-closure_3))

##########################################################

constructs:
	$(call test-file-parts,constructs,(test-constructs_1)(test-constructs_2a)(test-constructs_2b)(test-constructs_2c)(test-constructs_2d)(test-constructs_3))

# just the modules from constructs
only_constructs: constructs_1 constructs_2a constructs_2b constructs_2c
	         constructs_2d constructs_3

constructs_1:
	$(call test-file-parts,constructs,(test-constructs_1))

constructs_2a:
	$(call test-file-parts,constructs,(test-constructs_2a))

constructs_2b:
	$(call test-file-parts,constructs,(test-constructs_2b))

constructs_2c:
	$(call test-file-parts,constructs,(test-constructs_2c))

constructs_2d:
	$(call test-file-parts,constructs,(test-constructs_2d))

constructs_3:
	$(call test-file-parts,constructs,(test-constructs_3))

#################################################
errors:
	$(call test-file-parts,errors,(test-errors_1a)(test-errors_1b)(test-errors_2)(test-errors_3))

only_errors: errors_1a errors_1b errors_2 errors_3

errors_1a:
	$(call test-file-parts,errors,(test-errors_1a))

errors_1b:
	$(call test-file-parts,errors,(test-errors_1a))

errors_2:
	$(call test-file-parts,errors,(test-errors_2))

errors_3:
	$(call test-file-parts,errors,(test-errors_3))

#################################################

events:
	$(call test-file-parts,events,(test-events_1)(test-events_2)(test-events_3a)(test-events_3b)(test-events_3c)(test-events_4))

only_events: events_1 events_2 events_3a events_3b events_3c events_4

events_1:
	$(call test-file-parts,events,(test-events_1))

events_2:
	$(call test-file-parts,events,(test-events_2))

events_3a:
	$(call test-file-parts,events,(test-events_3a))

events_3b:
	$(call test-file-parts,events,(test-events_3b))

events_3c:
	$(call test-file-parts,events,(test-events_3c))

events_4:
	$(call test-file-parts,events,(test-events_4))

#################################################

# gc_3f first since it has the poorest performance
gc:
	$(call test-file-parts,gc,(test-gc_3f)(test-gc_1)(test-gc_2)(test-gc_3a)(test-gc_3b)(test-gc_3c)(test-gc_3d)(test-gc_3e)(test-gc_4))

only_gc: gc_3f gc_1 gc_2 gc_3a gc_3b gc_3c gc_3d gc_3e gc_4

gc_1:
	$(call test-file-parts,gc,(test-gc_1))

gc_2:
	$(call test-file-parts,gc,(test-gc_2))

gc_3a:
	$(call test-file-parts,gc,(test-gc_3a))

gc_3b:
	$(call test-file-parts,gc,(test-gc_3b))

gc_3c:
	$(call test-file-parts,gc,(test-gc_3c))

gc_3d:
	$(call test-file-parts,gc,(test-gc_3d))

gc_3e:
	$(call test-file-parts,gc,(test-gc_3e))

gc_3f:
	$(call test-file-parts,gc,(test-gc_3f))

gc_4:
	$(call test-file-parts,gc,(test-gc_4))

###################################################################

locals:
	$(call test-file-parts,locals,(test-locals_1)(test-locals_2))

only_locals: locals_1 locals_2

locals_1:
	$(call test-file-parts,locals,(test-locals_1))

locals_2:
	$(call test-file-parts,locals,(test-locals_2))

#####################################################################

math:
	$(call test-file-parts,math,(test-math_1)(test-math_2)(test-math_3)(test-math_4)(test-math_5))

only_math: math_1 math_2 math_3 math_4 math_5

math_1:
	$(call test-file-parts,math,(test-math_1))

math_2:
	$(call test-file-parts,math,(test-math_2))

math_3:
	$(call test-file-parts,math,(test-math_3))

math_4:
	$(call test-file-parts,math,(test-math_4))

math_5:
	$(call test-file-parts,math,(test-math_5))

#####################################################

nextvar:
	$(call test-file-parts,nextvar,(test-nextvar_1)(test-nextvar_2)(test-nextvar_3)(test-nextvar_4)(test-nextvar_5)(test-nextvar_6))

only_nextvar: nextvar_1 nextvar_2 nextvar_3 nextvar_4 nextvar_5 nextvar_6

nextvar_1:
	$(call test-file-parts,nextvar,(test-nextvar_1))

nextvar_2:
	$(call test-file-parts,nextvar,(test-nextvar_2))

nextvar_3:
	$(call test-file-parts,nextvar,(test-nextvar_3))

nextvar_4:
	$(call test-file-parts,nextvar,(test-nextvar_4))

nextvar_5:
	$(call test-file-parts,nextvar,(test-nextvar_5))

nextvar_6:
	$(call test-file-parts,nextvar,(test-nextvar_6))

##########################################################
sort:
	$(call test-file,sort)

# just for consistency
only_sort: sort

##########################################################

strings:
	$(call test-file-parts,strings,(test-strings_1)(test-strings_2)(test-strings_3)(test-strings_4))

only_strings: strings_1 strings_2 strings_3 strings_4

strings_1:
	$(call test-file-parts,strings,(test-strings_1))

strings_2:
	$(call test-file-parts,strings,(test-strings_2))

strings_3:
	$(call test-file-parts,strings,(test-strings_3))

strings_4:
	$(call test-file-parts,strings,(test-strings_4))

########################################################

vararg:
	$(call test-file-parts,vararg,(test-vararg_1)(test-vararg_2)(test-vararg_3)(test-vararg_4)(test-vararg_5))

only_vararg: vararg_1 vararg_2 vararg_3 vararg_4 vararg_5

vararg_1:
	$(call test-file-parts,vararg,(test-vararg_1))

vararg_2:
	$(call test-file-parts,vararg,(test-vararg_2))

vararg_3:
	$(call test-file-parts,vararg,(test-vararg_3))

vararg_4:
	$(call test-file-parts,vararg,(test-vararg_4))
