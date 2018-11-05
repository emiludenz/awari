module Awari
open System
type pit = {mutable cell:int;id:int}
type board = {pits:pit list}
type player = Player1 | Player2


let b = {pits= [for i in 0..13 -> {cell=3;id=i}]}
// Setting Home pits to zero
b.pits.[6].cell <-  0
b.pits.[13].cell <- 0

/// <summary>The function to draw our game board on screen</summary>
/// <param name="b">b: is a board holdning pits</param>
/// <remarks> At the moment it will only work with a specific size board</remarks>
/// <returns> A printed representation of the current game board</returns>
let printBoard (b:board) =
  /// Getting Graphic for this one!
  Console.BackgroundColor <- ConsoleColor.Black
  Console.ForegroundColor <- ConsoleColor.Red
  printfn """  
 █████╗ ██╗    ██╗ █████╗ ██████╗ ██╗
██╔══██╗██║    ██║██╔══██╗██╔══██╗██║
███████║██║ █╗ ██║███████║██████╔╝██║
██╔══██║██║███╗██║██╔══██║██╔══██╗██║
██║  ██║╚███╔███╔╝██║  ██║██║  ██║██║
╚═╝  ╚═╝ ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝"""
  Console.ResetColor()
  printfn "q to quit\n"
  
  // Starting to print board
  List.iter (fun x -> printf "%s" x) [for i in 1..6 -> "____"]
  printfn ""
  List.rev [for i in 1..6 -> i] |> List.iter (fun x -> printf "%3i|" x)
  printfn "P2"
  List.iter (fun x -> printf "%s" x) [for i in 1..6 -> "‾‾‾‾"]
  printfn ""
  // Printing player 1 board side
  Console.ForegroundColor <- ConsoleColor.Black
  Console.BackgroundColor <- ConsoleColor.DarkRed
  b.pits.[7..12] |> List.rev |> List.iter (fun x -> printf "%3i|" x.cell)
  printf "   "
  // Reseting the console to default
  Console.ResetColor()
  printfn ""
  // Printing players home pits
  Console.ForegroundColor <- ConsoleColor.Black
  Console.BackgroundColor <- ConsoleColor.DarkYellow
  printf "%-4i%23i" b.pits.[13].cell b.pits.[6].cell
  // Reseting the console to default
  Console.ResetColor()
  printfn ""
  // Printing player 2 board side
  Console.ForegroundColor <- ConsoleColor.Black
  Console.BackgroundColor <- ConsoleColor.DarkRed
  b.pits.[..5] |> List.iter (fun x -> printf "%3i|"  x.cell)
  printf "   "
  // Reseting the console to default
  Console.ResetColor()
  printfn ""
  List.iter (fun x -> printf "%s" x) [for i in 1..6 -> "____"]
  printfn ""
  List.iter (fun x -> printf "%3i|" x) [for i in 1..6 -> i]
  printfn "P1"
  List.iter (fun x -> printf "%s" x) [for i in 1..6 -> "‾‾‾‾"]
  printfn ""

/// <summary>The function will check if a pit belongs to a player</summary>
/// <param name="b">b: is a board holdning pits</param>
/// <param name="p">p: is a player</param>
/// <param name="i">i: is a pit</param>
/// <remarks> At the moment it will only work with a specific size board</remarks>
/// <returns> A bool</returns>   
let isHome (b:board) (p:player) (i:pit) : bool =
  if (i.id = b.pits.[6].id && p = Player1) then 
    true
  elif (i.id = b.pits.[13].id && p = Player2) then
    true
  else false

/// <summary>The function will check the baord to see if it's game over</summary>
/// <param name="b">b: is a board holdning pits</param>
/// <remarks> At the moment it will only work with a specific size board</remarks>
/// <returns> A bool</returns> 
let isGameOver (b:board) : bool =
  b.pits.[..5] |> List.forall (fun x -> x.cell = 0) || 
  b.pits.[7..12] |> List.forall (fun x -> x.cell = 0)

/// <summary>This function will distribute all them beans</summary>
/// <param name="b">b: is a board holdning pits</param>
/// <param name="p">p: is a player </param>
/// <param name="i">i: is pit </param>
/// <remarks> Has not been tested with anything but standard board size, 
/// but should work for almost any size</remarks>
/// <returns>A tuple of a changed board, the player and the pit</returns>
let distribute (b: board) (p:player) (i:pit) : board * player * pit =
  let mutable hand = i.cell
  i.cell <- 0
  // moveCount keeps track of placement, starting with the pit next to the chosen one!
  let mutable moveCount = i.id+1
  // 'move' makes sure to wrap around, so the list is more like an circle, 
  // and we need it both in the while loop and outside. The scope is important.
  let mutable move = moveCount%(b.pits.Length)

  while not(hand = 0) do
    move <- moveCount%(b.pits.Length)
    moveCount <- moveCount + 1
    if (isHome b p b.pits.[move]) then
      b.pits.[move].cell <- b.pits.[move].cell + 1
      hand <- hand-1

    elif (p=Player1) && not (isHome b Player2 b.pits.[move]) then
      b.pits.[move].cell <- b.pits.[move].cell + 1
      hand <- hand-1

    elif ( p=Player2) && not (isHome b Player1 b.pits.[move]) then
      b.pits.[move].cell <- b.pits.[move].cell + 1
      hand <- hand-1
  
  // Catching the opposing sides beans if the last bean is placed in 
  // an empty pit on the players home field. The opposing side will always be 12 minus the pit
  if(b.pits.[move].cell = 1 && b.pits.[move].id <= 5 && p = Player1) then
    b.pits.[6].cell  <- b.pits.[12-move].cell + b.pits.[6].cell + b.pits.[move].cell
    b.pits.[12-move].cell <- 0
    b.pits.[move].cell <- 0

  elif(b.pits.[move].cell = 1 && b.pits.[move].id >= 7 && p = Player2 && not(isHome b Player2 b.pits.[move])) then
    b.pits.[13].cell <- b.pits.[12-move].cell + b.pits.[13].cell + b.pits.[move].cell
    b.pits.[12-move].cell <- 0
    b.pits.[move].cell <- 0

  // Winner Winner Chicken Dinner! You get it all!
  if (isGameOver b) then 
    if (p=Player1) then
      let win = b.pits.[7..12] |> List.fold (fun acc x -> x.cell + acc) 0
      b.pits.[6].cell <- b.pits.[6].cell + win
      for i in 7..12 do
        b.pits.[i].cell <- 0
    else 
      let win = b.pits.[..5] |> List.fold (fun acc x -> x.cell + acc) 0
      b.pits.[13].cell <- b.pits.[13].cell + win
      for i in 1..5 do
        b.pits.[i].cell <- 0
  (b,p,b.pits.[move])


/// <summary>This function will get the players move</summary>
/// <param name="b">b: is a board holdning pits</param>
/// <param name="p">p: is a player </param>
/// <param name="q">q: is a string with the move </param>
/// <remarks> Has not been tested with anything but standard board size, 
/// but should work for almost any size</remarks>
/// <returns> A pit</returns>
let getMove (b:board) (p:player) (q:string) : pit =
  let mutable qInt = 0
  if (q = "q" || q = "Q") then Environment.Exit 1
  else qInt <- int32(q)-1
  // 6 could have been represented as a variable since it always (b.pits.Length/2) 
  // and 5 could have been (b.pits.Length/2)-1
  if (qInt >= 6) then qInt <- 5
  elif (qInt <= 0) then qInt <- 0
  // +7 could also have been a variable containg the value of "(b.pits.Length/2)+1" 
  // This way the game would scale well. 
  if p = Player1 then b.pits.[qInt]
  else b.pits.[qInt+7]


/// <summary>This function will get the players move</summary>
/// <param name="b">is a board holdning pits</param>
/// <param name="p">is a player </param>
/// <param name="q">is a string with the move </param>
/// <remarks> Has not been tested with anything but standard board size, 
/// but should work for almost any size</remarks>
/// <returns> A pit</returns>
let turn (b : board) (p : player) : board =
  let rec repeat (b: board) (p: player) (n: int) : board =
    printBoard b
    let str =
      if n = 0 then
        printf "%A's move? " p
        Console.ReadLine()
      else 
        printf "Extra Move %A " p
        Console.ReadLine()
    let i = getMove b p str
    let (newB, finalPitsPlayer, finalPit)= distribute b p i
    if not (isHome b finalPitsPlayer finalPit) 
       || (isGameOver b) then
      System.Console.Clear()
      newB
    else
      System.Console.Clear()
      repeat newB p (n + 1)
  repeat b p 0 


/// <summary>This function will play</summary>
/// <param name="b">is a board holdning pits</param>
/// <param name="p">is a player </param>
/// <remarks> Has not been tested with anything but standard board size, 
/// but should work for almost any size if scale is wanted</remarks>
/// <returns> A board</returns>
let rec play (b : board) (p : player) : board =
  if isGameOver b then
    b
  else
    let newB = turn b p
    let nextP =
      if p = Player1 then
        Player2
      else
        Player1
    play newB nextP