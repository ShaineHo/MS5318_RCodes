library(tidyverse)

df <- read.csv("gas_consumption.csv", header = T)

df %>% 
  filter(GroupNum %in% c(1,2))

df %>% 
  filter(GroupNum == 1 | GroupNum == 2)

df %>% 
  filter(GroupNum == c(1:2))

df[20,4]

gas.sub1 <- df[,3]

gas.sub2 <- df[1:10, -5]

head(gas.sub2)

gas.feb <- df %>% 
  filter(Month == "Feb")


#### IQ

iq <- read.csv("iq.csv", header = T)

iq.sub <- iq[c(15:30),]

df3 <- slice(iq, 15:30)

sd(iq.sub)

df3 %>% 
  unlist() %>% 
  sd

# how to create a data frame
customer <- data.frame('name' = c("John", "Mary"),
                       'spending' = c(1732, 2644),
                       'experience' = c(11,17))


str(iq)

summary(iq)

#### what the hell are u doing (stupid!)
iq %>% 
  unlist() %>% 
  as.numeric() %>% 
  mean()

mean(iq$IQ)


## assign new vaules to iq
iq[1:3,]= c(100, 101, 102)


softdrink<- read.csv("Lec7/Softdrink.csv")

softdrink %>%
  group_by(Brand.Purchased) %>% 
  count()


### Create matrix
A = matrix(1:6, nrow = 2, ncol = 3)
B = matrix(c(3, 1, 5, 4, 2, 8), nrow = 2, ncol = 3)
C = matrix(c(9, 5, 3, 2, 6, 8), nrow = 3, ncol =2)


## bind data
data1 = data.frame(name = 'Mike', income = 50)
data2 = data.frame(name = 'Jack', income = 60)
data3 = data.frame(experience = c(10,12))

data1 %>% 
  rbind(data2) %>% 
  cbind(data3)


sales1 = data.frame(brand = c("Coke", "Sprite"),
                    sales = c(109, 80))

sales2 = data.frame(brand = "Pepsi", sales = 95)

sales <- sales1 %>% 
  rbind(sales2)


iq$IQ %>% 
  hist(probability = T, break = 10)

hist(iq$IQ,breaks=c(0,1,2,3,4,5,10,20,max
                (iq$IQ)))

table(c(3, 4, 1, 1, 3,
        4, 3, 3, 1, 3, 
        2, 1, 2, 1, 2,
        3, 2, 3, 1, 1, 
        1, 1, 4, 3, 1)) %>% 
  pie(main = "Pie Chart")


table(c(3, 4, 1, 1, 3,
        4, 3, 3, 1, 3, 
        2, 1, 2, 1, 2,
        3, 2, 3, 1, 1, 
        1, 1, 4, 3, 1)) %>% 
  barplot(main = "barplot",
          xlab = "Category",
          ylab = "Count")

