// Name: Dona Maria
// netID: dmari21
// Project 2: Image processing in f#
// Description: This program implemets the five image processing function found in the F# library.
// Date: 10/28/2023

namespace ImageLibrary

module Operations =

 // This function converts the image to grayscale by calculating the average of red, green and blue values.
 // returns a new image (list of lists of tuples).
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    
    match image with
    | [] -> []
    | head :: tail ->
      let gray =
        head
        |> List.map (fun (r, g, b) ->
            let x = int (float r * 0.299 + float g * 0.587 + float b * 0.114)
            (x, x, x)
        )
      gray :: Grayscale width height depth tail
        


// This function increases the image seperation  
// If the rgb value is greater than the threshold the rgb value is set to 255 else 0
// returns an image (list of lists of tuples).
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    
    match image with
    | [] -> []
    | head :: tail ->
      let newTuple =
        head
        |> List.map (fun (r,g,b) ->
          let x =
            if (r <= threshold) then
              0
            else
              depth

          let y = 
            if (g <= threshold) then
              0
            else
              depth

          let z =
            if (b <= threshold) then
              0
            else
              depth
          (x,y,z)
        )
      newTuple :: Threshold width height depth tail threshold


// This function flips an image horizontally.
// returns an image (list of lists of tuples).
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    match image with
    | [] -> []
    | head :: tail -> List.rev head :: FlipHorizontal width height depth tail
    
// helper function for the edgeDetect function which takes in two pixels as the arguments
// calculates the distance between two pixels
// returns a list
  let distance p1 p2= 
    let(r1, g1, b1) = p1
    let(r2, g2, b2) = p2
    sqrt(float(r1 - r2)**2.0 + float(g1 - g2)**2.0 + float(b1 - b2)**2.0)

//  helper function for the edgeDetect function which takes in two lists as the arguments
// to find the pixel immediately to the right of the current pixel
// returns a list
  let rec rightDist row row_ =
    match row with
    | [] -> List.rev row_
    | head1 :: [] -> List.rev row_
    | head1 :: head2 :: tail -> rightDist(head2::tail) (distance head1 head2 :: row_)

//  helper function for the edgeDetect function which takes in two lists as the arguments
// to find the pixel immediately below the current pixel
// returns a list
  let belowDist row1 row2 =
    List.map2 (fun x y -> distance x y) row1 row2

// helper function for the edgeDetect function which takes in a list as the argument
// removes the last element in a List
// returns a list
  let rec lastElement list = 
    match list with
    | [] -> []
    | head::[] -> []
    | head :: tail -> head :: lastElement tail

// helper function for the edgeDetect function which takes in two values and threshold as the arguments.
// compares the values with the threshold
// returns a pixel
  let compare x y threshold=
    if x > float threshold || y > float threshold then
      (0, 0, 0)
    else
      (255, 255, 255)

// This function checks if the pixel is an edge and change it to (0,0,0) if it is and to (255,255,255) if it is not an edge
// returns an image (list of lists of tuples).
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    let l2 = []
    match image with
    | [] -> []
    | head1::[] -> [] 
    | head1 :: head2 :: tail -> //rightDist head1 l2 && lastElement(belowDist head1 head2)
      let result = List.map2 (fun x y -> compare x y threshold) (rightDist head1 l2) (lastElement (belowDist head1 head2))
      result :: EdgeDetect width height depth (head2::tail) threshold
      
// This function rotates an image 90 degrees to the right.        
// returns an image (list of lists of tuples).
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    List.transpose (List.rev image)

