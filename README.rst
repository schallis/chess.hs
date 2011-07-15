========
chess.hs
========
:Info: The beginnings of a game of chess written in Haskell
:Authors: Steve Challis (http://schallis.com)
:Requires: GHC
:License: MIT

Usage
=====
The game currently compute valid moves for each piece on the board. A king
piece has been created to play around with in the interpreter:::

    $ ghci chess.hs
    *Main> valid_moves testKing.
    [(0,1),(1,0),(1,1)]

