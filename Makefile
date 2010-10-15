.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -bool start_clean

MODS = component \
	   configuration \
	   connection \
	   directory \
	   exec_context \
	   manager \
	   node \
	   nvlist \
	   path \
	   port \
	   rtctree \
	   server \
	   ticker \
	   unknown \
	   utils

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump

