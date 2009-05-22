src = *.erl
obj = $(patsubst %.erl, %.beam, $(wildcard $(src)))

all: $(obj)

%.beam: %.erl
	erlc -W $<

clean:
	rm -rf *.beam erl_crash.dump

