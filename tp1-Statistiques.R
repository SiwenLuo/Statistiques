# d : Proba pour diacrete et densite pour continues
# p : fonction de repartition
# q : quantile
# r : simulation

histogrammePasFixe <- function(x) {
  nbr = length(x)
  nbrClasses = as.integer(1 + log2(nbr))
  x = sort(x)
  a0 = as.integer(x[1] - 0.025*(x[nbr] - x[1]))
  ak = as.integer(x[nbr] + 0.025*(x[nbr] - x[1]))
  largeur = as.integer(ak/nbrClasses)
  hist(x, prob = T, breaks = c(a0, ak, largeur))
  # hist(x, breaks = 'Sturges' )
}

# rbinom(number of observations, value of max, prob)
binom <- rbinom(5, 2, 0.4)
print(binom)

norm1 <- rnorm(30,2,3)
hist(norm1, prob=T)

norm2 <- rnorm(1000,2,3)
hist(norm2, prob=T)

unif1 <- runif(30,0,10)
hist(unif1,prob=T)

unif2 <- runif(1000,0,10)
hist(unif2,prob=T)

exp1 <- rexp(30,3)
hist(exp1,prob=T)

exp2 <- rexp(1000,3)
hist(exp2,prob=T)

