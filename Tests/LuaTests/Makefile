define test-file
	echo '(require "$(1).rkt") (test-$(1))' | racket -i
endef

define test-file-parts
	echo '(require "$(1).rkt") $(2)' | racket -i
endef

all: calls closure constructs error events locals math nextvar sort strings vararg

calls:
	$(call test-file-parts,calls,(test-calls_1)(test_calls_2)(test-calls_3))

closure:
	$(call test-file,closure)

constructs:
	$(call test-file-parts,constructs,(test-constructs_1)(test_constructs_2a)(test_constructs_2b)(test_constructs_2c)(test_constructs_2d)(test-constructs_3))

errors:
	$(call test-file,errors)

events:
	$(call test-file-parts,events,(test-events_1)(test-events_2)(test-events_3)(test-events_4))

locals:
	$(call test-file-parts,locals,(test-locals_1)(test-locals_2))

math:
	$(call test-file-parts,math,(test-math_1)(test-math_2)(test-math_3)(test-math_4)(test-math_5))

nextvar:
	$(call test-file-parts,nextvar,(test-nextvar_1)(test-nextvar_2)(test-nextvar_3)(test-nextvar_4)(test-nextvar_5)(test-nextvar_6))

sort:
	$(call test-file,sort)

strings:
	$(call test-file-parts,strings,(test-strings_1)(test-strings_2)(test-strings_3))

vararg:
	$(call test-file-parts,vararg,(test-vararg_1)(test-vararg_2))

