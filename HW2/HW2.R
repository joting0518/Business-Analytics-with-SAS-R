craps_game <- function(num_simulations, bet_size) {
  results <- vector(length = num_simulations)
  for (i in 1:num_simulations) {
    dice_num <- sample(1:6, 2)
    dice_total <- dice_num[1] + dice_num[2]
    
    if (dice_total == 7 | dice_total == 11) {
      bet_count <<- bet_size + bet_count
      results[i] <- TRUE
    } else if (dice_total == 2 | dice_total == 3 | dice_total == 12) {
      bet_count <<- bet_count - bet_size
      results[i] <- FALSE
    } else {
      record <- dice_total
      dice_num <- sample(1:6, 2)
      dice_total <- dice_num[1] + dice_num[2]
      while (record != dice_total) {
        if (dice_total == 7) {
          bet_count <<- bet_count - bet_size
          results[i] <- FALSE
          break
        } else {
          dice_num <- sample(1:6, 2)
          dice_total <- dice_num[1] + dice_num[2]
        }
      }
      if (record == dice_total) {
        bet_count <<- bet_size + bet_count
        results[i] <- TRUE
      }
    }
  }
  
  for (i in results) {
    if(i==TRUE){
      win <<- win + 1
    }else{
      lose <<- lose + 1
    }
  }
  expected_value <<-  (win/num_simulations * bet_size) + (lose/num_simulations * -bet_size )
  return(expected_value)
}
expected_value <-0
bet_count <- 0
win<-0
lose<-0
craps_game(3, 1)
#win
#lose
#bet_count


