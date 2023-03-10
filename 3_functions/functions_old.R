function.identificar <- function(df,x){
  cbind(df, TIEMPO=rep(x,nrow(df)))
}

function.minus <- function(df){
  names(df) <- tolower(names(df))
  names(df) <- gsub(x = names(df), pattern = "_tri", replacement = "")  
  return(df)
}





