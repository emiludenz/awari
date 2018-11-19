open Awari

//Creating board
let b = {
  pits= [for i in 0..13 -> {cell=3;id=i}]}
// Setting Home pits to zero
b.pits.[6].cell <-  0
b.pits.[13].cell <- 0

let p1 = Player1
let p2 = Player2

printfn "Testing printBoard:"
printfn "%s" "printBoard just prints the board and will be tested within other functions."
 
printfn "Testing isHome:"
printfn "Test 1: %b" ((isHome b p1 b.pits.[6]) = true)
printfn "Test 2: %b" ((isHome b p2 b.pits.[13]) = true)
printfn "Test 3: %b" ((isHome b p1 b.pits.[5]) = false)

printfn "Testing isGameOver:"
printfn "Test 1: %b" ((isGameOver b) = false)
for i in 0..5 do b.pits.[i].cell <- 0
printfn "Test 2: %b" ((isGameOver b) = true)
for i in 7..12 do b.pits.[i].cell <- 0
printfn "Test 3: %b" ((isGameOver b) = true)

printfn "Testing getMove:"
printfn "Test 1: %b" ((getMove b p1 "2") = b.pits.[1])
printfn "Test 2: %b" ((getMove b p1 "7") = b.pits.[5])
printfn "Test 3: %b" ((getMove b p1 "-1") = b.pits.[0])
printfn "Test 4: %b" ((getMove b p2 "2") = b.pits.[8])
printfn "Test 5: %b" ((getMove b p2 "7") = b.pits.[12])
printfn "Test 6: %b" ((getMove b p2 "-1") = b.pits.[7])

let b0 = {
  pits= [for i in 0..13 -> {cell=3;id=i}]}
// Setting Home pits to zero
b0.pits.[6].cell <-  0
b0.pits.[13].cell <- 0

let b01 = {
  pits= [for i in 0..13 -> {cell=3;id=i}]}
// Setting Home pits to zero
b01.pits.[6].cell <-  0
b01.pits.[13].cell <- 0

//Creating board for testing
let b1 = {
  pits= [for i in 0..13 -> {cell=3;id=i}]} 
b1.pits.[5].cell <-  0
b1.pits.[7].cell <-  4
b1.pits.[8].cell <-  4
b1.pits.[6].cell <-  1
b1.pits.[13].cell <- 0

//Creating board for testing
let b001 = {
  pits= [for i in 0..13 -> {cell=3;id=i}]} 
b001.pits.[5].cell <-  0
b001.pits.[7].cell <-  4
b001.pits.[8].cell <-  4
b001.pits.[6].cell <-  1
b001.pits.[13].cell <- 0

//Creating board for testing
let b2 = {
  pits= [for i in 0..13 -> {cell=3;id=i}]}
b2.pits.[11].cell <- 0
b2.pits.[12].cell <- 4
b2.pits.[0].cell <-  4
b2.pits.[6].cell <-  0
b2.pits.[13].cell <- 1

//Creating board for testing
let b3 = {
  pits= [for i in 0..13 -> {cell=3;id=i}]}
b3.pits.[8].cell <-  0
b3.pits.[9].cell <-  4
b3.pits.[10].cell <- 4
b3.pits.[11].cell <- 0
b3.pits.[1].cell <-  0
b3.pits.[12].cell <- 4
b3.pits.[0].cell <-  4
b3.pits.[6].cell <-  0
b3.pits.[13].cell <- 5


//Creating board for testing
let b4 = {
  pits= [for i in 0..13 -> {cell=3;id=i}]}
b4.pits.[2].cell <- 0 
b4.pits.[3].cell <- 4 
b4.pits.[4].cell <- 4 
b4.pits.[5].cell <- 0 
b4.pits.[7].cell <- 0 
b4.pits.[8].cell <- 4
b4.pits.[6].cell <- 6
b4.pits.[13].cell <-0

//Creating boards
let b5 = {
  pits= [for i in 0..13 -> {cell=0;id=i}]}
b5.pits.[5].cell <- 1
b5.pits.[8].cell <- 3

//Creating boards
let b6 = {
  pits= [for i in 0..13 -> {cell=0;id=i}]}
b6.pits.[5].cell <- 0
b6.pits.[6].cell <- 4
b6.pits.[8].cell <- 0

//Creating boards
let b7 = {
  pits= [for i in 0..13 -> {cell=0;id=i}]}
b7.pits.[12].cell <- 1
b7.pits.[3].cell <- 3

//Creating boards
let b8 = {
  pits= [for i in 0..13 -> {cell=0;id=i}]}
b8.pits.[12].cell <- 0
b8.pits.[13].cell <- 4
b8.pits.[3].cell <- 0

printfn "Testing distribute:"
printfn "Test 1: %b" ((distribute b0 p1 b0.pits.[5]) = (b1, p1, b1.pits.[8])) 
printfn "Test 2: %b" ((distribute b01 p2 b01.pits.[11]) = (b2, p2, b2.pits.[0]))
printfn "Test 3: %b" ((distribute b2 p2 b2.pits.[8]) = (b3, p2, b3.pits.[11]))
printfn "Test 4: %b" ((distribute b001 p1 b001.pits.[2]) = (b4, p1, b4.pits.[5])) 
printfn "Test 5: %b" ((distribute b5 p1 b5.pits.[5]) = (b6, p1, b6.pits.[6])) 
printfn "Test 6: %b" ((distribute b7 p2 b7.pits.[12]) = (b8, p2, b8.pits.[13])) 

printfn "Testing turn: since turn takes user-input it isn't tested."

printfn "Testing play: since play takes user-unput it isn't tested."


