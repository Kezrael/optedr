
getStart <- function(cross, min, max, val, sens_opt){
  if(length(cross)==1){
    if(round(cross[1]-min, 6)==0){
      if((val < sens_opt((max+cross[1])/2))){
        start <- F
      }
      else{
        start <- T
      }
    }
    else{
      if((val < sens_opt((min+cross[1])/2))){
        start <- T
      }
      else{
        start <- F
      }
    }
  }
  else if(val < sens_opt((cross[2]+cross[1])/2)){
    start <- F
  }
  else {
    start <- T
  }
  return(start)
}

getPar <- function(cross){
  return(length(cross)%%2 == 0)
}

getCross2 <- function(cross, min, max, start, par){
  if(par & start){
    cross2 <- c(min, cross, max)
  }
  else if(par & !start){
    cross2 <- cross
  }
  else if(!par & start){
    cross2 <- c(min, cross)
  }
  else{
    cross2 <- c(cross, max)
  }
  return(cross2)
}
