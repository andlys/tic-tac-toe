# Tic Tac Toe game

A terminal game for two players. Each player can be:
* a human (choice is inputted from terminal);
* a computer:
  * a stupid computer (choice is random)
  * an intelligent computer (choice is made according to a minimax tree):
    * ~~simple slightly improved AI (just makes some moves where it is likely to win)~~;
    * slightly improved AI (when AI is the first player, it plays better; when it is the second player, it plays worse);
    * ~~invincible AI (is designed to never lose).~~

The most interesting part is building a minimax tree. You can see the code at `src/minimax-tree.lisp`
Code is well commented, so check it out.  

## Plans:
1. Invincibility of AI is now only a claim and currently it is being developed and tested...
1. To better cases when AI is the second player (sometimes it makes terrible choices)
1. To transform code into three levels of AI: simple, smart, invincible.

## How to run quickly
One of the ways you can run this project on Ubuntu:
* install [Steel Bank Common Lisp](http://www.sbcl.org/platform-table.html):  
`sudo apt-get install sbcl`
* clone the code from this repository:  
`git clone https://github.com/andlys/tic-tac-toe`
* change current directory:  
`cd tic-tac-toe/src`
* run the project as script:  
`sbcl --script main.lisp`
* enjoy!  

Default configuration is:
* player X - human;
* player O - AI.  

However, you can change it easily as you please. Read 'src/player.lisp' for more info.  

Human input visually is made through numpad numbers.
