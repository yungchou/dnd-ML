
# Scatter Matrix
library(psych)
pairs.panels(states, scale=TRUE, cex.cor=5, stars=TRUE, ci=TRUE)

baby.names <-  mutate(baby.names, logCount=count/1000)

filter(baby.names, count==max(count))

baby.names <-
  mutate(group_by(baby.names,year,sex),rank=rank(count))

baby.names <- mutate(group_by(baby.names, year, sex),
                     Proportion = count/sum(count))

