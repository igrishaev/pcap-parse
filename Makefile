
pcap = mdf-kospi200.20110216-0.pcap


all: download compile run run-rev


compile:
	ghc parser


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
