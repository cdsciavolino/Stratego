# Stratego

## **Overview**
A summary of the rules are written below. For a comprehensive *Stratego* guide, see the [*Stratego* wiki](https://en.wikipedia.org/wiki/Stratego)

#### *Rules of Play*
*Stratego* is a strategy much like chess with a few caveats:
* You and your opponent **cannot** see each other’s pieces before attacking them
* Pieces can only be taken by pieces with a **rank higher than its own**
* Pieces can only move **one** tile **vertically or horizontally** (not diagonally)
* Each player attempts to **capture the** opponent’s immoveable **flag** piece

As with any game, there are of course, exceptions to the above rules: 
* A *scout* piece (rank 2) can move as a rook does in chess
* A *bomb* piece (rank 0) defeats every other piece, but is immovable
* A *miner* piece (rank 3) can diffuse enemy bombs
* A *spy* piece (rank 1) can defeat a *marshal* piece (rank 10) if the spy attacks the marshall 

#### *Pieces and Ranks*
* Rank 0 -  Bomb (B)
* Rank 1 - Spy (S)
* Rank 2 -  Scout (2) 
* Rank 3 - Miner (3)
* Rank 4 - Sergeant (4) 
* Rank 5 - Lieutenant (5)
* Rank 6 - Captain (6) 
* Rank 7 - Major (7) 
* Rank 8 - Colonel (8) 
* Rank 9 - General (9) 
* Rank 10 - Marshal (10) 
* Rank 11 - Flag (F) 

## **What we did**
Our team created a simple, easy to use, Terminal-based version of *Stratego* in OCaml 4.03.00 where a user can play against an medium-difficulty AI. Movement is based on a grid similar to a chess board, but we decided to use an (x, y) coordinate system rather than the alphanumeric labeled grid. 

## **Setup and Gameplay**
1. Download the stratego folder to your local machine.
2. Make sure you have OCaml 4.03.00 installed to your machine. 
3. Open up Terminal or Command Line.
4. Navigate to the stratego folder within Terminal/Command Line
5. Enter 'make' into the console
5. Enter 'make play' into the console

## **Team and Division of Labor**
* Chris Sciavolino (*board.ml, game.ml*)
* Dan Laine (*ai.ml*)
* Katie Gioioso (*ai.ml*)
* Ryan Feldman (*display.ml, main.ml*)

## **Screenshots**
<img src="/screenshots/startBoard.png" width="375" /> <img src="/screenshots/intro.png" width="375" /> <img src="/screenshots/movement.png" width="375" /> <img src="/screenshots/conflict.png" width="375" /> 
