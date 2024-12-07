setwd("H:/sta215")
#Load Packages
install.packages("haven")
library("readr")
stfinal <- read_delim("stfinal.csv")

#Descriptive Statistics
table(stfinal$presenceofblood)

table(stfinal$crying)

mean(stfinal$humor)
sd(stfinal$humor)
min(stfinal$humor)
max(stfinal$humor)

mean(stfinal$d_d)
sd(stfinal$d_d)
min(stfinal$d_d)
max(stfinal$d_d)

table(stfinal$fightscenes)

mean(stfinal$creature)
sd(stfinal$creature)
min(stfinal$creature)
max(stfinal$creature)
#Table
table(stfinal$presenceofblood, stfinal$crying)
chisq.test(stfinal$presenceofblood, stfinal$crying)
#Boxplot
lm(humor ~ d_d, data = stfinal)
aov(humor ~ d_d, data = stfinal)
summary(humor ~ d_d, data = stfinal)
boxplot(stfinal$humor, stfinal$d_d)

#Scatterplot
linear_plot <- plot(stfinal$creature, stfinal$fightscenes)
print(linear_plot)
meany <- mean(stfinal$creature)
meanx <- mean(stfinal$fightscenes)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(creature ~ fightscenes, data = stfinal)
summary(linear_relationship)
abline(linear_relationship, col = "red")

#Residual Plot
plot(stfinal$presenceofblood, residuals(linear_relationship))
plot(stfinal$crying, residuals(linear_relationship))
