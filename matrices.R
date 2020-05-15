d3 <- 1:3

max_of <- function(row_rolls, col_rolls) {
  mat <- matrix(0, nrow = length(row_rolls), ncol = length(col_rolls), dimnames = list(row_rolls, col_rolls))
  
  #r for rows
  for(r in 1:length(row_rolls)) {
    #c for cols
    for(c in 1:length(col_rolls)) {
      mat[r,c] <- max(row_rolls[r], col_rolls[c])
    }
  }
  mat
}

mat <- max_of(d3, d3)

mat[1, 1]
mat[1, 2]
mat
