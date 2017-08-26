# Tic Tac Toe game

A terminal game for two players. Each player can be:
* a human (choice is inputted from terminal);
* a computer:
  * a stupid computer (choice is random)
  * an intelligent computer (choice is made according to a minimax tree):
    * ~~simple AI (just makes some moves where it is likely to win);~~
    * invincible AI (is designed to never lose).

The most interesting part is building a minimax tree. You can see the code at `src/minimax-tree.lisp`
Code is well commented, so check it out.

Note: invincibility of AI is now only a claim and currently it is being tested...

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
However, you can change it easily as you please.  

Human input visually is made through numpad numbers.
