# A Haskell implementation of hangman

This chooses a random word between 5 to 9 characters from a wordlist located in
`/usr/share/dict/words` and requires that file to work. Then asks for user input
to guess the word. If you guess 7 incorrect characters then you lose. Guess all
the characters in the random word and you win!

```
git clone git@github.com:MattMSumner/hangman.git
cd hangman
cabal install --only-dependencies
cabal build
dist/build/hangman/hangman
```
