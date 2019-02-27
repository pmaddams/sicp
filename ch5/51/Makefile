# Exercise 5.51

NAME = lisp
BUILD = clang -Weverything -Werror
FILES = `find * -name "*.[c|h]"`

.PHONY: build
build:
	${BUILD} -o ${NAME} src/*.c

.PHONY: clean
clean:
	rm -f ${NAME}

.PHONY: fmt
fmt:
	clang-format -i ${FILES}

.PHONY: lint
lint:
	clang-tidy ${FILES} -- -I src

.PHONY: test
test:
	for f in test/*; do \
		${BUILD} -o test/_ -I src src/[!main]*.c $$f && \
		./test/_ && \
		rm test/_; \
	done

.SILENT:
