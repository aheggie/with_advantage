library(purrr)
library(tibble)

roll <- function(d = 20) {
  sample(1:d, 1)
}

with_advantage <- function(d = 20, roll_f=roll) {
  rolls <- c(roll_f(d), roll_f(d))
  max(rolls)
}

with_disadvantage <- function(d = 20, roll_f=roll) {
  rolls <- c(roll_f(d), roll_f(d))
  min(rolls)
}

disadvantage_of_advantage <- function(d = 20) {
  with_disadvantage(d = d, roll_f = with_advantage)
}

advantage_of_disadvantage <- function(d = 20) {
  with_advantage(d = d, roll_f = with_disadvantage)
}

disadvantage_of_advantage()

with_advantage()

roll()

map_int_f <- function(f, d, n) {
  map_int(1:n, ~f(d))
}

make_rolls <- function(d = 20, n = 10000) {
  tibble(
    plain = map_int_f(roll, d, n),
    # advantage = map_int_f(with_advantage, d, n),
    # disadvantage = map_int_f(with_disadvantage, d, n),
    disadvantage_of_advantage = map_int_f(disadvantage_of_advantage, d, n),
    advantage_of_disadvantage = map_int_f(advantage_of_disadvantage, d, n)
  )
}

rolls <- make_rolls(d = 20, n = 100000)

map(rolls, mean)

# a typical result - disadvantage+of+advantage is the preferred result
# > rolls <- make_rolls(d = 20, n = 100000)
# 
# > map(rolls, mean)
# $plain
# [1] 10.52271
# 
# $disadvantage_of_advantage
# [1] 11.18138
# 
# $advantage_of_disadvantage
# [1] 9.84981
