df <- data.frame(
  x=c(5, 3, 0, 4),
  y=c(4, 4, 1, 3)
  )

ht <- function(t0, t1, x) {
  t0 + t1 * x
}

jt <- function(t0, t1) {
  f <- function (x, y) {
    (ht(t0, t1, x) - y) ^ 2
  }
  sum(mapply(f, df$x, df$y)) / nrow(df) / 2
}

jt(0, 1)
ht(-1, 2, 6)
# for (x in seq(.3, .5, .01)) print(jt(1, x))