Printer | HP LaserJet 400 M401dn
Formatter Number | X6605EW
Max Print Quality | ProRes 1200




https://github.com/entropia/tip-toi-reveng/blob/master/docs/The_Code.pdf

The printed code
The codes consisting of a matrix of small dots, in which information in the form of

The rasterized Code
After coloring the grid the structure is visible. The red dots are the
uniform pitch, which is used for adjustment. They enclose each nine
Information points which here diagonal as light blue and white pair with offset
are shown. On the left side of the grid box there in the middle of an anomaly with
vertical displacement which is necessary to identify the orientation.

The grid is shown in color red and blue dots here. The blue dots
guidance only where actually would be the grid. The white dots are
the actual information points.

The structure
This results in the basic structure of a repeating 4x4 pattern
has the following structure.
RP stands for a dot and RA for the raster orientation (raster
Anomaly). WP stand for values-points include the information.

RP | RP  | RP  | RP
RP | WPp | WP7 | WP6
RA | WP5 | WP4 | WP3
RP | WP2 | WP1 | WP0

The points
Each of the points mentioned in the previous section (RP, RA, WP) consists of a 16x16
Pixel matrix, which is explained here.

The snap point (RP)
The dot sits central in the middle of the 16x16-pixel matrix.

 |0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F
0| | | | | | | | | | | | | | | | 
1| | | | | | | | | | | | | | | | 
2| | | | | | | | | | | | | | | | 
3| | | | | | | | | | | | | | | | 
4| | | | | | | | | | | | | | | | 
5| | | | | | | | | | | | | | | | 
6| | | | | | | | | | | | | | | | 
7| | | | | | | |X|X| | | | | | | 
8| | | | | | | |X|X| | | | | | | 
9| | | | | | | | | | | | | | | | 
A| | | | | | | | | | | | | | | | 
B| | | | | | | | | | | | | | | | 
C| | | | | | | | | | | | | | | | 
D| | | | | | | | | | | | | | | | 
E| | | | | | | | | | | | | | | | 
F| | | | | | | | | | | | | | | | 

The grid-alignment (RA)
This point differs as the only of the grid center from vertically, making it easy to
identify. The grid center continues to better understand the grid
located. It should be noted that the distance is exactly half a block size,
which is also unique to this point.

 |0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F
0| | | | | | | | | | | | | | | | 
1| | | | | | | | | | | | | | | | 
2| | | | | | | | | | | | | | | | 
3| | | | | | | | | | | | | | | | 
4| | | | | | | | | | | | | | | | 
5| | | | | | | | | | | | | | | | 
6| | | | | | | | | | | | | | | | 
7| | | | | | | |O|O| |X|X| | | | 
8| | | | | | | |O|O| |X|X| | | | 
9| | | | | | | | | | | | | | | | 
A| | | | | | | | | | | | | | | | 
B| | | | | | | | | | | | | | | | 
C| | | | | | | | | | | | | | | | 
D| | | | | | | | | | | | | | | | 
E| | | | | | | | | | | | | | | | 
F| | | | | | | | | | | | | | | | 

The values point
The value points to consecrate the grid from diagonally and can thus four different
States are taking (right-bottom, left-bottom, left-top, right-top). In the
specified sequence corresponds to that encoding the values 0, 1, 2 and 3. FIG.

Value 0

 |0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F
0| | | | | | | | | | | | | | | | 
1| | | | | | | | | | | | | | | | 
2| | | | | | | | | | | | | | | | 
3| | | | | | | | | | | | | | | | 
4| | | | | | | | | | | | | | | | 
5| | | | | | | | | | | | | | | | 
6| | | | | | | | | | | | | | | | 
7| | | | | | | |O|O| | | | | | | 
8| | | | | | | |O|O| | | | | | | 
9| | | | | | | | | |X|X| | | | | 
A| | | | | | | | | |X|X| | | | | 
B| | | | | | | | | | | | | | | | 
C| | | | | | | | | | | | | | | | 
D| | | | | | | | | | | | | | | | 
E| | | | | | | | | | | | | | | | 
F| | | | | | | | | | | | | | | | 


Value 1

 |0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F
0| | | | | | | | | | | | | | | | 
1| | | | | | | | | | | | | | | | 
2| | | | | | | | | | | | | | | | 
3| | | | | | | | | | | | | | | | 
4| | | | | | | | | | | | | | | | 
5| | | | | | | | | | | | | | | | 
6| | | | | | | | | | | | | | | | 
7| | | | | | | |O|O| | | | | | | 
8| | | | | | | |O|O| | | | | | | 
9| | | | | |X|X| | | | | | | | | 
A| | | | | |X|X| | | | | | | | | 
B| | | | | | | | | | | | | | | | 
C| | | | | | | | | | | | | | | | 
D| | | | | | | | | | | | | | | | 
E| | | | | | | | | | | | | | | | 
F| | | | | | | | | | | | | | | | 


Value 2

 |0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F
0| | | | | | | | | | | | | | | | 
1| | | | | | | | | | | | | | | | 
2| | | | | | | | | | | | | | | | 
3| | | | | | | | | | | | | | | | 
4| | | | | | | | | | | | | | | | 
5| | | | | |X|X| | | | | | | | | 
6| | | | | |X|X| | | | | | | | | 
7| | | | | | | |O|O| | | | | | | 
8| | | | | | | |O|O| | | | | | | 
9| | | | | | | | | | | | | | | | 
A| | | | | | | | | | | | | | | | 
B| | | | | | | | | | | | | | | | 
C| | | | | | | | | | | | | | | | 
D| | | | | | | | | | | | | | | | 
E| | | | | | | | | | | | | | | | 
F| | | | | | | | | | | | | | | | 


Value 3

 |0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F
0| | | | | | | | | | | | | | | | 
1| | | | | | | | | | | | | | | | 
2| | | | | | | | | | | | | | | | 
3| | | | | | | | | | | | | | | | 
4| | | | | | | | | | | | | | | | 
5| | | | | | | | | |X|X| | | | | 
6| | | | | | | | | |X|X| | | | | 
7| | | | | | | |O|O| | | | | | | 
8| | | | | | | |O|O| | | | | | | 
9| | | | | | | | | | | | | | | | 
A| | | | | | | | | | | | | | | | 
B| | | | | | | | | | | | | | | | 
C| | | | | | | | | | | | | | | | 
D| | | | | | | | | | | | | | | | 
E| | | | | | | | | | | | | | | | 
F| | | | | | | | | | | | | | | | 


Still unexplained
When analyzing the code, the value of the WPs shows in order from top left
right-bottom (höchswertigen to least significant).

202
223
033

The resulting sequence of 2.0.2.2.2.3.0.3.3 reads binary as
10.00.10.10.10.11.00.11.11.

With this procedure, all the code can be explained, however, produced the
Bit sequence regularly a number that is lower by exactly 4106, as the specified code.

      | B   | E   | H   | B   | E   | H   | B   | E   | H   | Binary           | Decimal |
      | 2   | 2   | 2   | 5   | 5   | 5   | 8   | 8   | 8   |                  |         |
      | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---              | ---     |
15065 | 2   | 0   | 2   | 2   | 2   | 3   | 0   | 3   | 3   | 0010101011001111 | 10959   |
15066 | 0   | 0   | 2   | 2   | 2   | 3   | 1   | 0   | 0   | 0010101011010000 | 10960   |


Furthermore, it is still unclear how the parity (first column B2) is calculated. Easy
XOR and ADD approaches are not anyway.

