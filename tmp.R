f <- function(x){
  g <- function(y){
    y+z
  }
  z <- 4
  a <- x + g(x)
  print(a)
}