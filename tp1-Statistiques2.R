histogrammePasFixe <- function(x) {
  #x = runif(30)
  nbr = length(x)
  nbrClasses = as.integer(1 + log2(nbr))
  x = sort(x)
  a0 = x[1] - 0.025*(x[nbr] - x[1])
  ak = x[nbr] + 0.025*(x[nbr] - x[1])
  largeur = (ak-a0)/nbrClasses
  hist(x, prob=T, breaks=seq(a0, ak, largeur))
  # hist(x, breaks = 'Sturges' )
  # w <- rexp(1000, 3)
}

histogrammeMemeEffectif <- function(x) {
  a0 = x[1] - 0.025*(x[nbr] - x[1])
  ak = x[nbr] + 0.025*(x[nbr] - x[1])
  #k = as.integer(1 + log2(nbr))
  k = 5
  breaks <- c(a0, quantile(x,seq(1,5-1)/k),ak)
  hist(x, prob=T, breaks=c(0,17,46,79,145,260))
  lines(density(x))
}

plot

