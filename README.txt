CPSC-599
CPSC 599 Retrogame Project

Test programs:

To assemble any of the test programs, cd to the test directory and then run:
dasm [test_name].asm -o[test_name].prg where [test_name] is the name of the test you wish to assemble
Then, run the test program by dragging the resulting prg file onto the xvic emulator window, double clicking the prg file, or running xvic [test_name].prg from terminal.

Alternatively, run any of the pre-assembled prg files in the prg directory in the same fashion as above (without needing to assemble them)

Test Programs:

Test 00 - Simple return statement program

Test 01 - Simple printing (prints VIC)

Test 02 - Simple loop using printing algorithm in Test 01

Test 03 - Simple program that takes W,A,S,D keyboard input and outputs a value depending on key pressed. Use Q to quit program.

Test 04 - Simple random number generation using a hardcoded seed value and data from one of the VIC registers. Outputs a character (changes if you change the seed) then quits

Test 05 - Program that changes text, background, and border colour of VIC 20 console

Test 06 - Simple sound test with keyboard input.  Uses W,A,S,D to play sounds, Q to quit.

Test 07 - Changing the character layout of the vic-20. We can use this for graphics or level design. By defining our own graphics characters. For example, try hitting m (shows up as a black square instead of an M). Use enter to quit

Test 08 - Simple mirrored play field using characters

Test 09 - Tests basic left/right character movement, based on code from tests 03 and 07.  Uses A and S as input to move left/right.

Test 09-1 - Simple collision detection, extension of test 09.  Use W,A,S,D to move, Q to quit.

Test 09-2 - Expanded test 09, included movement of up and down (W and S).  Only goes from 0-255, so not entire screen.  Use W,A,S,D to move, Q to quit.  Todo: expand to full screen.

Test 10-1 - Simple collision animation based off Test 09-2.  Character 'B' changes colour and bounces back when it collides with 'C'. W,A,S,D to move, Q to quit.
