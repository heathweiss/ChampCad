module Examples.Cubical.CubicalKey where
{- 

terms::::::::
key: piece that will fit in the key way.
keyway: Holds the key. 
cubical structure: Structure, such as cubical shoe lift, that contains the keyway.

Build a cubical shape, from multiple cubes, and insert a keyway inside of it.

Keyway/Key
-Should the key(way) have straight walls or should they be same shape as cubical structure.


Keyway:
-Height: Be the full height of the cubical structure.
-Width:
 -Using a x1/x2 set of values
 -The KeywayDimensions x1 and x2 must be within the bounds of the cubical structure, along with a buffer.
  -Buffer on each side of keyway could be 1/2 width of keyway.
 -Using a single set of x1/x2 values will result in:
   -straight walls
   -having to know the x1/x2 values that will fit inside.

 -Using a width
  -The initial x1/x2 values could be calc'd in code, as the will be the 1st set that fit inside the cubical structure, along with the buffers.
  -Once these initial values are calculated, then they can be passed into the recursive call that builds the keyway.
  -

Key:
-Height: Be able to choose the height, as multiple keys may be used.
 -Will have flat top/bottom.
-Length:
 -Will have to be calc'd from the cubical structure, as the start/end is a function of key <= structure - ( (buffer * 2) - keyWidth )


Example: Build a shoe lift, using cubical system, that has a long narrow keyway inside of it.

An example of building a lift with cubical, but no keyway is in: Examples/Cubical/SandalsAFO
-Has to be altered for a new cubical system.

The Plan:::::::::::::::::::::::::

Create a key as a list of cubes, the same length as the cubical structure into which it will be inserted.
-It should(will?) have a consistent width.
-The width of the ends of key will be > the first(or more) widths of surrounding cubical structure, and <  last(or less) widths of the cubical structure.

===using zip===
Zip the keyway and cubical structure together as follows:
If the keyway is not within the bounds of the cubical structure, then create the current cube form the left and right values of the containing structure.
If keyway is within bounds, use the keyway left/right faces, +++ the containing structure left/right faces to create the 2 sets of cubes required.
The question is, will they be back faces or front faces.
-Normally when building cubes, start with a single back face and +++> a list of front faces onto it.
-Assume the cubical structure will  be wide enough for its entire lenght, minus the ends:
-So I have to determine if this is the first face, and so a back face, or if the is one of the following front faces. How?
--Perhaps use stated to know this. So use recursion instead, to make use of the states. See recursion section.

===using recursion===
Continue on from =zip= section, with idea of states.
Use the following data structures and functions
-}

--The state of the cubical structure
data KeyState =
 InitialState {keyWidth::Double, bufferWidth::Double}
 --Supply width of key, and amount of buffer on each side of key.

--the width keyway as given by the left and right x values.
data KeywayDimensions = KeyDimensions {x1::Double, x2::Double}

--And use the set of recursive functions that will pattern match on the KeyState constructors
{-will need more work before it can compile
buildCubicalKeyway :: KeyState -> 
buildCubicalKeyway
  (InitialState left right) 
-}
