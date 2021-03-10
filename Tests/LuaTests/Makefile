define test-file
	echo '(require "$(1).rkt") (test-$(1))' | racket -i
endef

define test-file-parts
	echo '(require "$(1).rkt") $(2)' | racket -i
endef

all: calls closure constructs errors events gc locals math nextvar sort strings vararg

calls:
	$(call test-file-parts,calls,(test-calls_1a)(test-calls_1b)(test-calls_1c)(test-calls_2a)(test-calls_2b)(test-calls_3a)(test-calls_3b)(test-calls_3c)(test-calls_4)(test-calls_5)(test-calls_6))

closure:
	$(call test-file-parts,closure,(test-closure_1)(test-closure_2)(test-closure_3))

constructs:
	$(call test-file-parts,constructs,(test-constructs_1)(test-constructs_2a)(test-constructs_2b)(test-constructs_2c)(test-constructs_2d)(test-constructs_3))

errors:
	$(call test-file-parts,errors,(test-errors_1a)(test-errors_1b)(test-errors_2)(test-errors_3))

events:
	$(call test-file-parts,events,(test-events_1)(test-events_2)(test-events_3a)(test-events_3b)(test-events_3c)(test-events_4))

gc:
        $(call test-file-parts,events,(test-gc_1)(test-gc_2)(test-gc_3a)(test-gc_3b)(test-gc_3c)(test-gc_3d))

locals:
	$(call test-file-parts,locals,(test-locals_1)(test-locals_2))

math:
	$(call test-file-parts,math,(test-math_1)(test-math_2)(test-math_3)(test-math_4)(test-math_5))

nextvar:
	$(call test-file-parts,nextvar,(test-nextvar_1)(test-nextvar_2)(test-nextvar_3)(test-nextvar_4)(test-nextvar_5)(test-nextvar_6))

sort:
	$(call test-file,sort)

strings:
	$(call test-file-parts,strings,(test-strings_1)(test-strings_2)(test-strings_3)(test-strings_4))

vararg:
	$(call test-file-parts,vararg,(test-vararg_1)(test-vararg_2)(test-vararg_3)(test-vararg_4)(test-vararg_5))

