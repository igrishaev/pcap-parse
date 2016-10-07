
pcap = mdf-kospi200.20110216-0.pcap


all: cabal download compile run run-rev


compile:
	ghc parser


.PHONY: cabal
cabal:
	cabal install binary


run: $(pcap)
	./parser $(pcap)


run-rev: $(pcap)
	./parser $(pcap) -r


download:
	wget "http://www.tsurucapital.com/file/$(pcap).gz"
	gunzip $(pcap).gz


.PHONY: clean
clean:
	rm *.pcap
	rm *.gz
