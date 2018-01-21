## ------------------------------------------------------------------------
n <- 13
p <- 0.7
x <- 0:n

plot(x, dbinom(x, size = n, prob = p), main = "Probability mass function for Bin(13,0.7)")

## ------------------------------------------------------------------------
plot(x, pbinom(x, size = n, prob = p), type="s", main = "Cumulative distribution function for Bin(13,0.7)")

## ------------------------------------------------------------------------
p_seq <- seq(from = 0, to = 1, length = 101)
plot(p_seq, qbinom(p_seq, size = n, prob = p), type="s", main = "Quantile function for Bin(13,0.7)")

## ------------------------------------------------------------------------
draws <- rbinom(100, size = n, prob = p) # draw 100
brks  <- (0:(n+1)) - 0.5
hist(draws, breaks = brks, main = "Random draws from Bin(13,0.7)") 

## ------------------------------------------------------------------------
hist(draws, breaks = brks, probability = TRUE)
points(x, dbinom(x, size = n, prob = p), col="red")

## ------------------------------------------------------------------------
rate <- 23
x <- 0:40 # with no upper limit we need to decide on an upper limit

plot(x, dpois(x, lambda = rate), main = "Probability mass function for Po(2)") 

## ------------------------------------------------------------------------
plot(x, ppois(x, lambda = rate), type="s", main = "Cumulative distribution function for Po(2)")

## ------------------------------------------------------------------------
plot(p_seq, qpois(p_seq, lambda = rate), type="s", ylim=c(0,40), main = "Quantile function for Po(2)") # Change the y limits for comparison purposes

## ------------------------------------------------------------------------
draws <- rpois(999, lambda = rate)

hist(draws, breaks = (0:(max(draws)+1)) - 0.5, probability = TRUE, main = "Random draws from Po(s)")
points(x, dpois(x, lambda = rate), col="red")

## ------------------------------------------------------------------------
a <- 13
b <- 65

# The curve function expects you to give a function of `x` and then it 
# (internally) creates a sequence of values from `from` and to `to` and creates
# plots similar to what we had before, but using a line rather than points.
curve(dunif(x, min = a, max = b), from = -1, to = 2,
      xlab='y', ylab='f(y)', main='Probability density function for Unif(0,1)')

## ------------------------------------------------------------------------
curve(punif(x, min = a, max = b), from = -1, to = 2,
      xlab='y', ylab='F(y)', main='Cumulative distribution function for Unif(0,1)')

## ------------------------------------------------------------------------
curve(qunif(x, min = a, max = b), from = 0, to = 1,
      xlab='p', ylab='F^{-1}(p)', main='Quantile function for Unif(0,1)')
## ------------------------------------------------------------------------
random_uniforms <- runif(999, min = a, max = b)
hist(random_uniforms, probability = TRUE, main = "Random draws from Unif(13,65)")
curve(dunif(x, min = a, max = b), add = TRUE, col="red")

## ------------------------------------------------------------------------
mu    <- -4
sigma <- sqrt.3 # standard deviation

curve(dnorm(x, mean = mu, sd = sigma), # notice the 3rd argument is the sd
      from = -4, to = 4,
      main = "PDF for a standard normal")

## ------------------------------------------------------------------------
curve(pnorm(x, mean = mu, sd = sigma), 
      from = -4, to = 4,
      main = "CDF for a standard normal",
      ylab = "F(x)")

## ------------------------------------------------------------------------
curve(qnorm(x, mean = mu, sd = sigma),
      from = 0, to = 1, 
      main = "Quantile function for a standard normal")

## ------------------------------------------------------------------------
draws <- rnorm(100, mean = mu, sd = sigma)
hist(draws, probability = TRUE)
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "red")


die = c(1,2,2,3,3,4)
rolls = expand.grid(die1 = die, die2 = die, die3 = die)
print(rolls)
sum = rowSums(rolls); tsum = table(sum)
print(sum)
print(sum)
dragonwood3 = data.frame(x = round(as.numeric(names(tsum)),0))
pmf = round(as.numeric(table(sum)/length(sum)),3)
print(pmf)                         
die = c(1,2,2,3,3,4)
rolls = expand.grid(die1 = die, die2 = die, die3 = die)
sum = rowSums(rolls); tsum = table(sum)
dragonwood3 = data.frame(x = round(as.numeric(names(tsum)),0),
                         pmf = round(as.numeric(table(sum)/length(sum)),3)) %>%
  mutate(cdf = cumsum(pmf))
t(dragonwood3)
die = c(1,2,2,3,3,4)
rolls = expand.grid(die1 = die, die2 = die, die3 = die)
sum = rowSums(rolls); tsum = table(sum)
dragonwood3 = data.frame(x = round(as.numeric(names(tsum)),0),
                         pmf = round(as.numeric(table(sum)/length(sum)),3))
  mutate(cdf = cumsum(pmf))
t(dragonwood3)
die = c(1,2,2,3,3,4)
rolls = expand.grid(die1 = die, die2 = die, die3 = die)
sum = rowSums(rolls); tsum = table(sum)
dragonwood3 = data.frame(x = round(as.numeric(names(tsum)),0),
                         pmf = round(as.numeric(table(sum)/length(sum)),3))
  mutate(cdf = cumsum(pmf))
t(dragonwood3)
print(sum)
print(tsum)
print(table(sum))
print(dragonwood3)
print(pmf)
?pmf
??pmf
print(round(as.numeric(table(sum)/length(sum)),3))
print(table(sum)/length(sum))
Print(as.numeric(table(sum)/length(sum)),3)
print(as.numeric(table(sum)/length(sum)),3)
sum
a = 30
b = 50
a-b
c = 50
d = 70
c+d
print(c+d)
f = 2
g = 89
f+g
a=50
b=58
a+b

