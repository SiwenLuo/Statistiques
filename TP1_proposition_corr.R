#Partie I
#Q1)

MABOXMULLER <- function (n)
    {
        m <- 2*ceiling(n/2)
        U <- runif(m)#On tire autant d'uniformes qu'il faut ( n+1
                     #si n est impair ) 

        R=-2*log(U[1:(m/2)])
        R=sqrt(R)

        T=2*pi*U[(m/2+1):m]

        X=R*cos(T)
        Y=R*sin(T)

        res<- c(X,Y)#NB: notons qu'on a fait un choix arbitraire de l'ordre dans lequel on remplit le vecteur
                 # ...comme les composantes sont des gausiennes indépendantes, peu importe.

       return (res[1:n])


    }

#Q3)

#Simulation de la gaussienne par inversion de la fonction de répartition
SIMUNORMINVFDR <- function (n,m,s)
    {
        U <- runif(n)

        X <- qnorm(U,mean=m,sd=s)

        return (X)

    }

#Simulation des trois échantillons
Xa <- 2+2*MABOXMULLER(10000)
Xb <- SIMUNORMINVFDR(10000,2,2)
Xc <- rnorm(10000,mean=2,sd=2)

#On utilise la fonction hist mais on ne trace pas
ha=hist(Xa,plot=FALSE)
hb=hist(Xb,plot=FALSE)
hc=hist(Xc,plot=FALSE)

#On trace les histogrammes sous forme de lignes pour plus de lisibilité
plot(ha$mids,ha$density,type="l")
lines(hb$mids,hb$density,col=4)
lines(hc$mids,hc$density,col=14)

legend(-4,0.18,legend=c("Box Muller","inv. f.d.r.", "rnorm"),col=c("black",4,14),lty=c(1,1,1))

#Partie II
#Q2)

x <- seq(-7,7,by=0.01)

f <- function (x)
    {
        0.5*dnorm(mean=-3,sd=1,x)+0.5*dnorm(mean=2,sd=2,x)}

plot(x,f(x),type="l")

#Q4)
tirageXcond <- function (n)
    {
        U <- runif(n)

        X=rnorm(mean=-3,sd=1,n)*(U<0.5)+rnorm(mean=2,sd=2,n)*(U>0.5)

        return(X)
    }

x1 <- tirageXcond(10000)

hist(x1,prob=TRUE,ylim=c(0, 0.22))

lines(x,f(x),col=4)

#Q5)
plot(x,3*dnorm(x,mean=0, sd=3),type="l",col=14)
lines(x,f(x))

#Q6)
TIRAGEMELANGEREJET <- function()
    {
        #Initialisation à des valeurs arbitraires pour passer dans la boucle while une première fois
        U=2

        test=0

        while (U>test)
            {
                Y <- rnorm(1,mean=0,sd=3)
                U <- runif(1)
                test=f(Y)/dnorm(Y,mean=0,sd=3)/3
            }

        return (Y)
    }
        
#Q7)
x2=NULL
for (i in 1:10000)
    x2=c(x2, TIRAGEMELANGEREJET())

hist(x2,prob=TRUE,ylim=c(0,0.22))

lines(x,f(x),col=4)

#On retrace l'histogramme de x1 sous forme de ligne pour plus de lisibilité
hx1=hist(x1,plot=FALSE)

lines(hx1$mids,hx1$density,col=14)

legend(3,0.2,legend=c("densité f","hist. de x1"),col=c(4,14),lty=c(1,1,1))


