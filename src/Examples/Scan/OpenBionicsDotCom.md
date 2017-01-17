OpenBionics.com socket and adaptor.

================================ create socket =======================================
Socket with a round end onto which the adaptor will fit.

[rawMDR] -----transpose(+3) . reduceRows 35 ----------> [innerMDR]

[innerMDR] ------transpose(+3)--------------------------[outerMDR]

[innerMDR]----------- createVerticalWalls---------->    [CornerPoints: socket w/o adaptor]


================================ adaptor ==========================================
The adaptor which will fit between the socket and the hand.

[Base of hand 
 measurements:
 [Radius]       ------map (transpose +3) ------->[outerWallRadii]------         
           ]

[outerRadii
 innterRadii
 Angles       ---- Primitives.Cylindrical.Walled.cylinder----> [CornerPoints: base of hand cylinder]
 Origin
 Height    ]