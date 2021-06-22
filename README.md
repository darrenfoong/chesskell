# chesskell

When I was young, I read a newspaper article about a teenager who built
a chess program in Visual Basic (!), but I didn't have the skills to do
it back then.

I decided to write a chess program in Haskell to fulfil a childhood
goal, as well as to:

- Write a non-trivial program in a functional programming language.
  I learnt [ML](https://en.wikipedia.org/wiki/ML_(programming_language))
  in university but it was (to quote a lecturer) "good only for
  [counting change (page 52)](https://www.cl.cam.ac.uk/teaching/1213/FoundsCS/fcs-notes.pdf)".
  I chose Haskell because "what are [monads](https://en.wikipedia.org/wiki/Monad_(functional_programming))"?
- Write a web application in Haskell, using [Yesod](https://www.yesodweb.com/).
  I was too comfortable with the Spring Framework and wanted to try
  something different. It turned out to be _very_ different, and this
  program admittedly doesn't do persistence and Yesod just serves as a
  simple user interface for the chess engine.
- Implement the [minimax algorithm](https://en.wikipedia.org/wiki/Minimax)
  and [alpha-beta pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning).
  While I had an idea of how they worked, one doesn't really know how
  an algorithm works until one works through it, or implement it, or
  even better, implement it in a functional programming language.
  
The program is laughably slow and weak at chess. A list of to-dos:

1. Improve the web user interface (and stop play after checkmate).
2. Improve performance.
3. Improve the web user interface to make it look nicer in Chrome
   (currently tested only in Firefox).
4. Make the command line interface work (this was before Yesod came
   along).
5. Implement persistence to host multiple sessions/games.

## Formatting

```
ormolu -i $(find . -name '*.hs')
hlint src
```

## Building

```
stack build
stack test
```

## Running

```
stack run chesskell     # command line UI
stack run chesskell-web # web UI at localhost:3000
```
