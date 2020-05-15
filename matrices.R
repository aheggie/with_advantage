#define functions
d <- function(n) {1:n}

advantage_of <- function(rolls) {
  max_of(rolls, rolls)
}

max_of <- function(row_rolls, col_rolls) {
  function_of(row_rolls, col_rolls, max)
}

disadvantage_of <- function(rolls) {
  min_of(rolls, rolls)
}

min_of <- function(row_rolls, col_rolls) {
  function_of(row_rolls, col_rolls, min)
}

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

#code


base_rolls <- d(3)
rolls_expected <- mean(base_rolls)
advantage_of_disadvantage <- advantage_of( disadvantage_of(base_rolls) )
advantage_of_disadvantage
disadvantage_of_advantage <- disadvantage_of(advantage_of(base_rolls))
mean(advantage_of_disadvantage)
mean(disadvantage_of_advantage)
