# DISCRET : 
# Variables qualitatives, proportionnelle a la frequence relative de chaque modalite

# La quantite de chaque categorie
x <-c(2,2,3,4,5,5,6)
# Les noms des categorie
partis <- c("one", "two", "three", "four", "five", "six", "seven")
barplot(x, names=partis) # Frequenes sur les coordonnees
pie(x,labels=partis) # Les proportions


# Variables quantitatives, utlise pluton les diagrammes en batons car elles ont un ordre naturel
barplot(c(235,183,285,139,88,67,3),names=c(0,1,2,3,4,5,6))


# CONTINUE : Faut ordonner les donnees en premier
x <- c(91.6, 35.7, 251.3, 24.3, 5.4, 67.3, 170.9, 9.5, 118.4, 57.1)
y <- sort(x)
print(min(x))
print(y)

# les deux sont egaux
l <- log(22,base=2)
l2 <- log(22)/log(2)
print(l)
print(l2)


# Meme largeur
# Histogramme -> une estimation de la densite des observations
hist(y, prob=T, breaks = seq(0,260,52)) # bornes + largeur de chaque classe
# Polygone de repartition empirique
abs<-c(0,26,78,130,182,234,275) #les valeurs du milieu de chaque rectangle
ord<-c(0.0082,0.0077,0.0058,0.0019,0.0019,0.0019,0) # Les valeurs correspondantes + deux bornes sur Y
hist(y, prob=T, breaks=seq(0,260,52), xlim=c(0,300), ylim=c(0,0.009)) # l'intervalle x et y
lines(abs,ord,lwd=3) # tracer la courbe

# Meme effectf
# Histogramme : la moyenne de deux bornes
hist(y, prob=T, breaks = c(0,17,46,79,145, 260)) # il suffit d'ajouter les intervalles de classes
# les courbes plus lisses avec le noyau -> density(x)
lines(density(y))


# Fonction de repartition empirique -> graphes de probabilites
plot(ecdf(y)) # Ceci n'est pas possible de verifier a l'oeil si c'est pas alinee




# Passer aux graphes de probabilistes

# La loi uniforme a mon avis F(x) = P(X<=x) = (b-x)/(b-a) => (xi,i/n)
unif <- runif(300,0,5)
plot(sort(unif)[1:299],seq(1:299)/300)

# Loi exponentiel : y=-lambda*x
# (xi, log(1-i/n)) -> i est seq(1:9) ou il y a 10 elements en totale, donc diviser par n=10
plot(sort(x)[1:9],log(1-seq(1:9)/10),ylim=c(-2.5,0.1))
abline(v=0)
abline(h=0)

# Loi normal
# (xi, Phi(-1)(i/n)) : i de 1 a n-1, n est le nombre d'elements, Phi(-1) donner par qnorm
plot(sort(x)[1:9], qnorm(seq(1:9)/10))
abline(h = 0)

# Tracer presque la meme chose que precedent, utile pour vite faire
qqnorm(x)

# Moyenne empirique
moyenne <- mean(y)
print(moyenne)

# Les valeurs extremes
min <- min(y)
max <- max(y)
moyenneLocalisation <- (min+max)/2
print(moyenneLocalisation)

# Mediane empirique : qui n'est pas impacte par les valeurs extremes
median(y) # 62.2

# Variance
sn_au_carree <- var(y)
# Ecart-type
sn <- sd(y)
# coefficient de variance empirique
coe <- sn/moyenne
print(coe)

# Quantile(x,p)
quan <- quantile(y,1/2)
print(quan) # Affichage : 50%, et la medianne : 62.2

# minimum, premier quartile, mediane, moyenne, troisieme quartile, maximun
summary(y)



