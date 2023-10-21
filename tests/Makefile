# Get wsf-assemble from wslib and wsc from whitespace-rs
# https://github.com/thaliaarchi/wslib
# https://github.com/CensoredUsername/whitespace-rs
WSF_ASSEMBLE = wsf-assemble
WSA_ASSEMBLE = wsc

WSF = $(patsubst ./%,%,$(shell find . -name '*.wsf'))
WS = $(patsubst %.wsf,%.ws,$(WSF))

.PHONY: all
all: $(WS)

%.ws: %.wsa
	$(WSA_ASSEMBLE) -f asm -t -o $@ $<

%.wsa: %.wsf
	$(WSF_ASSEMBLE) $< $@
