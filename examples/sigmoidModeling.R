
# Prediction of performance evolution in learning a determinist trajectory

for (level in seq(1,30,1)) {
  if (level >20) {
    lambda = 5
  } else {
    lambda = 5*level/20
  }
  performance = data.table(x=seq(-5,5,0.1), y=sapply(seq(-5,5,0.1), function(x) { return(sigmoid(x, c(1, lambda, -5)) * 0.75 + 0.25) } ))
  toPlot = ggplot(performance, aes(x=x,y=y)) + geom_point()
  print(toPlot)
}


