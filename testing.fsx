open Awari

let mutable bB = {pits= [for i in 0..13 -> {cell=3;id=i}]}
// Setting Home pits to zero
bB.pits.[6].cell <-  0
bB.pits.[13].cell <- 0


let mutable bA = {pits= [for i in 0..13 -> {cell=3;id=i}]}
// Setting Home pits to zero
bA.pits.[6].cell <-  0
bA.pits.[13].cell <- 0



let restart (b: board) : board = 
    for i in 0..13 do
        b.pits.[i].cell <- 3
    b.pits.[6].cell <-  0
    b.pits.[13].cell <- 0
    b


printBoard (play bB Player1)



bB.pits.[5].cell <-  10
bB.pits.[12].cell <- 10
printBoard (play bB Player1)


bB <- restart bB