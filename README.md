This program parses trading data placed in special PCAP files. It is written in
Haskell and depends only on `binary` semi-standard package.

The program uses lazy byte strings to not load entire file in memory. Passing a
wrong PCAP file (without such UDP packets) returns an empty result.

Installation steps:

1. Clone this repo.
2. Install Haskell libraries with `make cabal`.
3. Compile a program: `make compile`.
4. Download sample PCAP file: `make download`.
5. Run the program: `make run` to see the result.

To change the order of lines, pass `-r` flag or run `make run-r`.

See the output in `output.txt` and `output-r.txt` files. Use them as unit tests
(changing code should not create diffs on that files).
