# 'not in' operator
"%nin%" <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}
