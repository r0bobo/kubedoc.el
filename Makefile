all: test lint compile doctor

test:
	eldev test -u on,text

lint:
	eldev lint

compile:
	eldev compile --warnings-as-errors

doctor:
	eldev doctor
