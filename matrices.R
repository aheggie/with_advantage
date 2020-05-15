d3 <- 1:3

#function of compares one vector of rolls with another via some comparison function f
function_of <- function(row_rolls, col_rolls, f) {
  mat <- matrix(0, nrow = length(row_rolls), ncol = length(col_rolls), dimnames = list(row_rolls, col_rolls))
  
  #r for rows
  for(r in 1:length(row_rolls)) {
    #c for cols
    for(c in 1:length(col_rolls)) {
      mat[r,c] <- f(row_rolls[r], col_rolls[c])
    }
  }
  mat
}

max_of <- function(row_rolls, col_rolls) {
  function_of(row_rolls, col_rolls, max)
}

advantage <- function(rolls) {
  max_of(rolls, rolls)
}

mat <- advantage(d3)

mat[1, 1]
mat[1, 2]
mat
