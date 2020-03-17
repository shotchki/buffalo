key <- rbind(c(1,2,3,4),c(5,6,7,8),c(9,10,11,12),c(13,14,15,16))

play <- rbind(c(1,2,3,4),c(12,6,11,8),c(7,5,9,10),c(14,15,13,16))

dup <- rbind(c(1,2,3,4),c(1,6,11,8),c(7,5,9,10),c(14,15,13,16))

check_duplicates <- function(x) {
  if(sum(duplicated(as.vector(x))) > 0) {
    message <- paste("Grid contains", sum(duplicated(as.vector(x))), "duplicates")
  } else {
    message <- "Grid is duplicate free"
  }
  return(message)
}
  
score_grid <- function(key,test) {
  for (n in 1:4){
    buff_col <- sum(key[,n] == test[,n])
    cow_col <- sum(key[,n] %in% test[,n]) - sum(key[,n] == test[,n])
    print(paste("Column", n, "contains", buff_col, "buffaloes and", cow_col, "cows"))
  }
  for (n in 1:4){
    buff_row <- sum(key[n,] == test[n,])
    cow_row <- sum(key[n,] %in% test[n,]) - sum(key[n,] == test[n,])
    print(paste("Row", n, "contains", buff_row, "buffaloes and", cow_row, "cows."))
  }
}

checks(key,play)
