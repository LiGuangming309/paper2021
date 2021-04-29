#test
beta <- 0.013018
epa = function(X, b=beta) {(exp(b*X)-1)*all_bmr}

min = 0
max = 30
X = seq(min, max, 0.5)
rates = data.frame(x=X, epa=epa(X))

plot(rates$x, rates$epa)