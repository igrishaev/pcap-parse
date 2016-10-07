
pcap = mdf-kospi200.20110216-0.pcap


all: cabal compile download run


compile:
	ghc parser


.PHONY: cabal
cabal:
	cabal install binary


run: $(pcap)
	./parser $(pcap)


run-r: $(pcap)
	./parser $(pcap) -r


download:
	wget "http://www.tsurucapital.com/file/$(pcap).gz"
	gunzip $(pcap).gz


dump: $(pcap)
	./parser $(pcap)> output.txt
	./parser $(pcap) -r > output-r.txt


.PHONY: clean
clean:
	rm *.pcap
	rm *.gz
