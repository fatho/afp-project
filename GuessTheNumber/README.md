# Documentation GuessTheNumber
## Encryption
### Key Location
The key file is read from `config/rsa_key` relative to the current directory of the cgi application, which should be usually the directory where `main.cgi` is located. The key is stored in a proprietary binary format. The creation of such file is covered by the next section.
## Key Generation
If you ran the `make`-script previously, there should be an executable named `genkey.bin` in the project directory.
When run without arguments, `genkey.bin` creates a new 1024 bit key and stores it in a file named `rsa_key` in the current directory. Alternatively, you can supply the desired filename as a command line argument:

```
./genkey.bin [output-file]`
```

The key generator can also be run without compiling it first using `runhaskell`:

```
runhaskell genkey/GenKey.hs [output-file]`
```