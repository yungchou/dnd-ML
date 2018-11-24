#DATA LOAD
data <- read.csv('Height_data.csv')
height <- data$Height

hist(height) #histogram

#POPULATION PARAMETER CALCULATIONS
pop_sd <- sd(height)*sqrt((length(height)-1)/(length(height)))
pop_mean <- mean(height)

zz <- (72 - pop_mean) / pop_sd

p_yellow1 <- pnorm(72, pop_mean, pop_sd)    #using x, mu, and sigma
p_yellow2 <- pnorm(z)                       #using z-score of 2.107
Python

p_blue1 <- 1 - p_yellow1   #using x, mu, and sigma
p_blue2 <- 1 - p_yellow2   #using z-score of 2.107

# Using scale
scale(x)
(x-mean(x))/sd(x)

names(iris)
test <- iris %>%
  group_by(Species) %>%
  mutate_at(vars(colnames(iris[-5])),funs(Z=scale(.)))

library(plotly)

plot_ly(x=iris$Species, type = "histogram", name = "Histogram") %>%
  add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))

plot(test[6:9])

Now visualize the z-scores: The first graph below is the raw data. The second is the ungrouped z-scores--we've just rescaled the data to an overall mean=0 and SD=1. The third graph is what your code produces. Each group has been individually scaled to mean=0 and SD=1.

gridExtra::grid.arrange(
grobs=setNames(names(dat)[c(3,5,4)], names(dat)[c(3,5,4)]) %>%
map(~ ggplot(dat %>% mutate(group=paste(Species,variable,sep="_")),
aes_string(.x, colour="group")) + geom_density()),
ncol=1)