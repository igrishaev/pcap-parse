
pcap = mdf-kospi200.20110216-0.pcap

compile:
	ghc parser

run:
	./parser $(pcap)
