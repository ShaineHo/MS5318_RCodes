capm1 <- read.csv("Lec10/capm.csv", header = T)
attach(capm1)

g1 = lm(Sony.Change ~ Market.Change) 

g2 = lm(Sony.Change~Market.Change, data=capm1[-60,]) 

g3 = lm(Sony.Change~Market.Change, data=capm1[-100,])

g4 = lm(Sony.Change~Market.Change, data=capm1[-c(60,100),])


plot(Market.Change, Sony.Change)
abline(g1$coefficients)
abline(g2$coefficients, col="red")
abline(g3$coefficients, col="green")
abline(g4$coefficients, col="blue")


############################################################

g1 <- lm(Sony.Change ~ Market.Change) 

par(new=TRUE)
plot(Market.Change, Sony.Change)
abline(coef = c(g1$coefficients, g2$coefficients, g3$coefficients, g4$coefficients), 
       col = c("black", "red", "green", "blue"))

g2 = lm(Sony.Change~Market.Change, data=capm[-60,]) 
abline(g2$coefficients, col="red")

g3 = lm(Sony.Change~Market.Change, data=capm[-100,])
abline(g3$coefficients, col="green")
g4 = lm(Sony.Change~Market.Change, data=capm[-c(60,100),])
abline(g4$coefficients, col="blue")


##############################################################

require(stats)
sale5 <- c(6, 4, 9, 7, 6, 12, 8, 10, 9, 13)
plot(sale5)
abline(lsfit(1:10, sale5))
abline(lsfit(1:10, sale5, intercept = FALSE), col = 4) # less fitting
