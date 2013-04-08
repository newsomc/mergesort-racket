## Merge sort - Racket
Racket wizard [Bill Abresch](http://github.com/wija) and I implemented a version of Merge sort in Racket. 


1. <strong>merge-sort-racket-vectors.rkt</strong> was implemnted using native Racket vector functions to access and write data. Short, simple, and the fastest of all our attempts.
2. <strong>merge-sort-structs-and-special-casing.rkt</strong>. This version uses Racket structs to manage subvectors as well as special casing for subvectors of length 1, 2 and 3. The second fastest.
3. <strong>merge-sort-custom-subvectors.rkt</strong> An early try. Shows how to use functions to create subvector indeces from a single input vector. 

## Todo:
* Dip down to insert sort after we see a pattern of 16 items
* Clarify which optimizations made sense, and which did not.

## Data

### Racket Vectors
items | cpu-ms | real-ms | (cpu-ms/items) * 1.0

  (2 11 13 0 5.5)  
  (4 9 10 0 2.25)  
  (8 9 11 0 1.125)  
  (16 13 14 0 0.8125)  
  (32 9 11 0 0.28125)  
  (64 13 15 0 0.203125)  
  (128 10 12 0 0.078125)  
  (256 10 11 0 0.0390625)  
  (512 15 17 0 0.029296875)  
  (1024 10 12 0 0.009765625)  
  (2048 11 12 0 0.00537109375)  
  (4096 13 14 0 0.003173828125)  
  (8192 15 16 0 0.0018310546875)  
  (16384 31 33 0 0.00189208984375)  
  (32768 41 43 0 0.001251220703125)  
  (65536 70 72 0 0.001068115234375)  
  (131072 126 128 0 0.0009613037109375)  
  (262144 972 976 690 0.0037078857421875)  
  (524288 1180 1190 701 0.00225067138671875)   
  (1048576 1744 1758 773 0.0016632080078125)  
  
### Structs and special casing

##### No Special Casing
###### Even numbers of items.
(2 11 15 0 5.5)  
(4 11 15 0 2.75)  
(8 15 19 0 1.875)  
(16 17 21 0 1.0625)  
(32 18 21 0 0.5625)  
(64 16 17 0 0.25)  
(128 19 22 0 0.1484375)  
(256 20 23 0 0.078125)  
(512 19 22 0 0.037109375)  
(1024 14 15 0 0.013671875)  
(2048 27 29 0 0.01318359375)  
(4096 13 17 0 0.003173828125)  
(8192 45 50 0 0.0054931640625)  
(16384 44 48 0 0.002685546875)  
(32768 63 68 0 0.001922607421875)  
(65536 100 104 0 0.00152587890625)  
(131072 177 182 0 0.00135040283203125)  
(262144 1440 1454 1078 0.0054931640625)  
(524288 1806 1820 1107 0.003444671630859375)  
(1048576 2709 2752 1263 0.0025835037231445312)  

##### Special case for 2 
###### Even numbers of items
(2 19 22 0 9.5)  
(4 19 23 0 4.75)  
(8 16 20 0 2.0)  
(16 10 14 0 0.625)  
(32 35 41 0 1.09375)  
(64 16 20 0 0.25)  
(128 14 18 0 0.109375)  
(256 18 22 0 0.0703125)  
(512 18 23 0 0.03515625)  
(1024 19 23 0 0.0185546875)  
(2048 20 24 0 0.009765625)  
(4096 40 45 0 0.009765625)  
(8192 20 24 0 0.00244140625)  
(16384 41 44 0 0.00250244140625)  
(32768 59 71 0 0.001800537109375)  
(65536 95 99 0 0.0014495849609375)  
(131072 161 166 0 0.00122833251953125)  
(262144 304 308 0 0.00115966796875)  
(524288 1707 1717 1084 0.0032558441162109375)  
(1048576 2458 2482 1193 0.0023441314697265625)


##### With special cases for 2 and 3
###### Odd numbers of items
(3 27 41 0 9.0)  
(5 11 12 0 2.2)  
(9 11 11 0 1.2222222222222223)  
(17 10 11 0 0.5882352941176471)  
(33 14 15 0 0.42424242424242425)  
(65 11 12 0 0.16923076923076924)  
(129 11 11 0 0.08527131782945736)  
(257 10 11 0 0.038910505836575876)  
(513 10 12 0 0.01949317738791423)  
(1025 11 11 0 0.010731707317073172)  
(2049 11 13 0 0.005368472425573451)  
(4097 14 15 0 0.003417134488650232)  
(8193 18 18 0 0.0021969974368363236)  
(16385 27 28 0 0.001647848642050656)  
(32769 48 49 0 0.0014647990478806189)  
(65537 78 79 0 0.0011901673863619025)  
(131073 142 142 0 0.0010833657580127105)  
(262145 277 279 0 0.0010566671117129833)  
(524289 1187 1189 605 0.00226401850887583)  
(1048577 1840 1845 651 0.0017547590687188447)  
