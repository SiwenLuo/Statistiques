
generateur_nombres_aleatoires <- function(){
  x <- runif(30, 0, 10)
  hist(x)
  # rnorm(n,mu,sigma)
  # add=TRUE dans la fonction curve() superimpose la densite sur l???histogramme.
  # dnorm(x,mean=mu,sd=sigma) donne une evaluation de la densite de proba
  y <- rnorm(5000,35,5)
  hist(y, probability = TRUE)
  curve(dnorm(x,mean=35,sd=5), add=TRUE, col="blue")
}


wait_time <- function(){
  uniforme <- runif(1000, 0, 10)
  for (i in 0 : 1000){
    x <- runif(10, 0, 10)
    t <- max(uniforme)
    marius <- c()
    jeannette <- c()
    marius <- append(marius, t-2*(mean(x)))
    #print(marius)
    jeannette <- append(jeannette,t-max(x))
    #print(jeannette)
  }
  plot(marius)
  plot(jeannette)
}