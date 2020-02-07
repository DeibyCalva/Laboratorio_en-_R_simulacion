## PREGUNTA 1 ##

defectuosos = rbinom(24, 25, 0.15)
defectuosos
any(defectuosos>5)
sum(defectuosos>5)

# P=0.2
defectuosos = rbinom(24, 25, 0.2)
defectuosos
any(defectuosos>5)
sum(defectuosos>5)

#P=0.25
defectuosos = rbinom(24, 25, 0.25)
defectuosos
any(defectuosos>5)
sum(defectuosos>5)


## PREGUNTA 2 ##
binsim=rbinom(10000,20,0.3)
length(binsim[binsim<=5])/length(binsim)
pbinom(5,20,0.3)

sum(binsim==5)/length(binsim)
dbinom(5,20,0.3)

mean(binsim)
20*0.3

var(binsim)
20*0.3*0.7

#con funcion quantile y con el qbinom 
quantile(binsim, prob=c(95,99,99.9999)/100)
qbinom(0.95,20,0.3)

qbinom(0.99,20,0.3)

qbinom(0.999999,20,0.3)



## PREGUNTA 3 ##

r=rbinom(n=100,size=18,p=0.76)
mean(r)
var(r)

###PREGUNTA 4#
ranbin <- function(n, size, prob){
  cumpois <- pbinom(0:(size - 1), size, prob)
  singlenumber <- function(){
    x <- runif(1)
    N <- sum(x > cumpois)
    N
  }
  replicate(n, singlenumber())
}
system.time(gcFirst = TRUE, ranbin(n = 1000, size = 10, prob = 0.4))
system.time(gcFirst = TRUE, ranbin(n = 10000, size = 10, prob = 0.4))
system.time(gcFirst = TRUE, ranbin(n = 100000, size = 10, prob = 0.4))

### PREGUNTA 5###
ranbin2 <- function(n, size, prob){
  singlenumber <- function(size, prob) {
    x <- runif(size)
    N <- sum(x < prob)
    N
  }
  replicate(n, singlenumber(size, prob))
}

system.time(gcFirst = TRUE, ranbin2(n = 10000, size = 10, prob = 0.4))
system.time(gcFirst = TRUE, ranbin2(n = 10000, size = 100, prob = 0.4))
system.time(gcFirst = TRUE, ranbin2(n = 10000, size = 1000, prob = 0.4))

