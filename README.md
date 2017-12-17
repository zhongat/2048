# 2048

Our game is an extension of the classical game - 2048 written in OCaml

Created by Stephanie Chang, Anita Sharma, Yun Ping Tseng, and Amy Zhong

A final project for Cornell's [CS 3110: Data Structures and Functional Programming, Fall 2017](http://www.cs.cornell.edu/courses/cs3110/2016fa/) 

## Installation

This requires OCaml 4.03.0

Note: This game does not work on Macs unless you have XQuartz11 installed. To install XQuartz, go to this website:
[XQuartz Installation](https://www.xquartz.org/)

## How to Run

### Launching the game

Navigate to your directory for 2048 and run `make play`.

### Playing the game
Launch the game and press '1' to play the classic one-player version of 2048.

Press '2' to play the multiplayer version of 2048. In this version, two players race to get the 2048 tile. If both players are unable to move, the player with the highest score wins.

Press 'a' to watch our AI solve 2048.

Press 'c' to play against our AI. You win if you are able to get a higher score than the AI in less moves. You lose if you cannot move before the AI fills up the board.

#### Controls
Player 1: 

'w' - up

'a' - left

's' - down

'd' - right

Player 2: 

'i' - up

'j' - left

'k' - down

'l' - right

### Testing and Cleaning
Run `make` or `make test` to compile and run test cases.

Run `make clean` to remove build files.
