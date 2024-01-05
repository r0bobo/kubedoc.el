all: test lint compile doctor

test:
	eldev test

lint:
	eldev lint

compile:
	eldev compile --warnings-as-errors

doctor:
	eldev doctor
