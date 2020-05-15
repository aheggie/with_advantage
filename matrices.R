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

#extra credit functions

pct_above_n <- function(n, vec) {
  sum(vec>n)/length(vec)
}

pct_above_n_s <- function(max_n, vec) {
  purrr::map_dbl(1:max_n, ~pct_above_n(., vec))
}

#code

roll_info <- function(d) {
  base_rolls <- d(d)
  
  advantage <- advantage_of(base_rolls)
  disadvantage <- disadvantage_of(base_rolls)
  
  advantage_of_disadvantage <- advantage_of( disadvantage )
  disadvantage_of_advantage <- disadvantage_of(advantage)
  expected_values = list(
    base = mean(base_rolls),
    advantage = mean(advantage),
    disadvantage = mean(disadvantage),
    advantage_of_disadvantage = mean(advantage_of_disadvantage),
    disadvantage_of_advantage = mean(disadvantage_of_advantage)
  )
  
  #extra_credit
  max_difficulty = d - 1;
  win_table = tibble(
    to_best = 1:max_difficulty,
    base = pct_above_n_s(max_difficulty, base_rolls),
    advantage_of_disadvantage = pct_above_n_s(max_difficulty, advantage_of_disadvantage),
    disadvantage_of_advantage = pct_above_n_s(max_difficulty, disadvantage_of_advantage)
    )
  
  list(expected_values = expected_values, win_table = win_table)
}

roll_info(d = 3)
roll_info(d = 20)

