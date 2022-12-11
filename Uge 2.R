
#In a binomial distribution. What is the chance of getting 1 time (x) of something , if doing "two trials" ##!!! SIMULATION   ###!!!
dbinom(x = 1,size = 3,prob = 0.5)
dbinom(x = c(1,2,3,4),size = 2,prob = 0.5)


#10 flips of 100 coins, hvor der er 0.5 chance for succes hver gang. tallet der kommer ud af hvor mange succes
out= rbinom(n = 10,prob = 0.5,size = 100)
plot(1:10,cumsum(out))


#-------------------------------------SIMULATION--------------------------------------
#Manuel sim
p <- 0.1
nRepeat <- 30
#den er bionomal så vi siger svar er 0 eller 1
tmp <- sample(c(0,1), prob = c(1-p,p),size=nRepeat,replace=TRUE)
####->  getting sum ->>>  sum(tmp==1)
table(tmp)

#programmed sim
rbinom(n = 2,size = 30,prob = c(1-p,p))





##konk
#rbinom -> random simulation
#dbinom -> udregner density function
#pbinom ->udregner distribution function INDTIL og med det punkt eg P(X<=5) for succeses. Så dette er chance for at få 5 eller mindre succes

##AKA disse to er det samme
pbinom(q = 4,size = 10,prob = 0.6)
sum(dbinom(x = c(1,2,3,4),size = 10,prob = 0.6))









n=10
x=3
choose(n,x)


pbinom(10,10,0.6)

pbinom(4,10,0.6)
1-pbinom(4,10,0.6)



#---------------------------------------------------------------------------
#HYPERGEO

n <- 3
N <- 20
a <- 2


mean <- n*a/N
varians <- (n*a*(N-a)*(N-n))/(N^2*(N-1))

#P(X=>1) = 1 - P(X = 0) 

x <- 0
P1 = choose(a,x)*choose(N-a,n-x)/choose(N,n)
PLargerThan1 <- 1-P1

#og med r-kommandoer
n <- 3
N <- 20
a <- 2
x <- 0
phyper(q = 0,m = 2,n = 18,k = 3)

#-------------------------------------------------------------


#1
phyper(10,99,1,10)

x <- 10
n <- 10
a <- 99
N <- 100
  
choose(a,x)*choose(N-a,n-x)/choose(N,n)
#2

phyper(10,99,1,10)

x <- 10
n <- 10
a <- 90
N <- 100

choose(a,x)*choose(N-a,n-x)/choose(N,n)


#----------------------------------------------------------------


dbinom(10,10,0.99)
dbinom(10,10,0.90)


#----------------------------------------------------------------

x <- c(0:5)
lambda <- 1.6

tæthed <- exp(-lambda)*(lambda^x)/factorial(x)
1-sum(tæthed)


x <- c(0:8)
lambda <- 1.6*5

tæthed <- exp(-lambda)*(lambda^x)/factorial(x)
sum(tæthed)

#----------------------------------------------------------

x <- 0:19
lambda <- 5*180/60

tæthed <- exp(-lambda)*(lambda^x)/factorial(x)
1-sum(tæthed)

#12% for at den bliver exceeded PER 5 min

qpois(p = 0.99,lambda)
#Should be 25% or more
#OR
poises <- ppois(c(0:100),lambda)
c(0:100)[poises >= 0.99]
#aka mindste er 25!!


#---------------------------------------------
#den tager spredning og ikke varians.. dumt
1-pnorm(4000,70*55,sqrt(10^2*55))
