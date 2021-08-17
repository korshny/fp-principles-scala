def combos(n: Int) = {
  if n == 0 then println("End")
  else for (x <- 0 to n) combos(n - 1)
}
combos(5)