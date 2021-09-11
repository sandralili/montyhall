#' @title
#'   Create a new Monty Hall Problem game.
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#' @param ... no arguments are used by the function.
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#' @examples
#'   create_game()
#'@export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Contestant Selects a door - Function
#' @description
#'  In this function of the program, de contestant choose their first door.
#' @details
#'  The function will create a vector with 3 doors. ONE door is randomly selected. 
#' @param 
#'  Three numbers are placed on a vector (representing each door, 1, 2, and 3).
#' @return 
#'  The game returns the selection (pick) of the contestant. 
#'  Door: 1 goat
#'  Door: 2 goat
#'  Door: 3 door
#' @examples
#'  select_door( )
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Host opens a "goat" door - Function
#' @description
#'  In this function the host will be opening a door always 
#'  (the exception is if the contestant already opened a "car" door). 
#'  Hence, the door that the host will open is not a "door" car
#'  and no the already chosen door by the contestant.
#' @details
#'  If the contestant already got a "car" door in the previous function, 
#'  the host can open one of the two remaining doors. But if the contestant 
#'  got a "goat" door in the first selection, the host will open a "goat" door.
#'  "a.pick" variable will be saving the "goat" or "car" options.
#' @param 
#'  Using the "if" function, the function will open the "goat" or "car" door. 
#'  If the the first pick was a "goat" the "if" function will choose to open 
#'  another door. If the first pick was a "car" door then the if statement, 
#'  will be opening one of the reminding doors.
#' @return 
#'  The function could return a door with a "goat" door , or a "car" door 
#'  depending of the first choice/results. If first door was "goat" then the 
#'  host door will be always a door with the other "goat"
#' @examples
#'  open_goat_door( )
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Changing doors - Function
#' @description
#'  This function will allow the contestant to change their first pick if 
#'  they wanted to.
#' @details
#'  This function will be using the variables "stay", "opened door", and 
#'  the "a.pick", this variable was previously used in the previous function. 
#'  The contestant will decide if they want to stay or get another pick. 
#' @param 
#'  The function "if" will help to run different commands depending on the 
#'  contestant choice. If the contestant selects to "stay" then his previous 
#'  pick will be remain the same (and will be a final pick). If the contestant 
#'  chooses to change the door then the function "if" will run the code to 
#'  choose a different door from the remaining ones, and this will be his final pick. 
#' @return 
#'  The function will be returning the final pick of the contestant, (either if it is still their first choice o a new choice).
#' @examples
#' change_door( )
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if contestant has won - Function
#' @description
#'  This function will help to determine if the contestant has chosen the 
#' "car" door, or if the contestant did not. A message will be displayed the
#'  "WIn" or "LOSE" words.
#' @details
#'  The function will be utilizing the variables "final.pick" and "game" 
#'  previously used in another function. 
#' @param 
#'  The function "if" will help to run the code depending on the choice of 
#'  the contestant. If the contestant chose the door with a car. then the "if"
#'  function will print in the screen "WIN" if the contestant choice was 
#'  a door with a goat, the "if" function will run the code printing the "LOSE"
#'  message.
#' @return 
#'  Depending on the final pick of the contestant, the word "WIN" or "LOSE" 
#'  will be displayed.
#' @examples
#'  determine_winner( )
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title 
#'  Play Game - Function
#' @description
#' This function will help to demonstrate which is the best strategy, 
#' "stay" or "switch" 
#' @details
#'  The function will utilize all the previous functions, to create the 
#'  game, to select the first door, to the host open one door, to the 
#'  contestant to decides to stay or choose another door. The contestant option 
#'  will be saved in a variable, also if the contestant's final choice, their
#'  strategy, the opened doors numbers and the game outcome.
#' @param 
#'  This function will allocate variables to store the game setup (create game), 
#'  a variable to store the initial pick, another one to store the opened doors, another
#'  one to save the final pick, another one for the outcome (stay or switch), another 
#'  variable to save the "strategy", and another for the outcome.
#' @return 
#'  The function will display the game setup (cars and goats order), the initial 
#'  selection, the opened doors, the final door selection and the results of the 
#'  game "WIN" or "LOSE".
#' @examples
#'  play_game( )
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}




#' @title
#'  Play "n" games - Function
#' @description
#'  This function will be using the play_game() function to simulates and test 
#'  the game a hundred times. The game will be using a loop to run the function "n" 
#'  times (n=100 in this came).
#' @details
#'  The function will be using a loop that will help to running the game continually
#'  up to 100 times to see the different results. 
#' @param 
#'  The function will run the game utilizing the function ""loop.count""play_game()", 
#'  the library (dplyr), the function "for(in)". Some of the variables used in this 
#'  function are loop.count, results.list, and game.outcome.
#' @return 
#'  The function will return the outcome of the strategy chosen. How many times a 
#'  contestant could win if chooses to stay or how may times the contestant could 
#'  win if chooses to switch?
#' @examples
#'  play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
