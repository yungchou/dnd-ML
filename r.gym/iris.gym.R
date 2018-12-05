str(iris)

# Get the data type of each variable
var <- sapply(iris,class)
vartype <- lapply(unique(var), function(x){names(var[var==x])})
names(vartype) <- unique(var); vartype

var.datatype

# NORMALIZATION
# A z-score of 2.0 means the measurement is 2 standard
# deviations away from the mean.
library(dplyr)
summary(iris)
iris_norm <- iris %>% mutate_at(vars(var.datatype$numeric),
                   funs('norm' = (.-mean(.))/sd(.)))
summary(iris_norm[5:9])

library(psych)
# Examine linearity and correlation of numberic variables
pairs.panels(iris_norm, cex.cor=5,
#             smooth=TRUE,    # a loess fit
             lm=TRUE,         # the linear fit
             las=1, ci=TRUE, density=TRUE, stars=TRUE
             )

library(plotly)
p3d <- plot_ly() %>%
  add_trace(data = iris, type="scatter3d",
            x = ~Sepal.Length,
            y = ~Petal.Length,
            z = ~Petal.Width,
            color = ~Sepal.Length,
#            colors=c("green","blue"),
            mode = "markers",
            marker=list(size=10,opacity=0.5,
                        line=list(color='dark gray',width=1))) %>%
  layout(title = "Iris 3D Scatter",
         scene = list(
         xaxis = list(title = "Sepal.Length"),
         yaxis = list(title = "Petal.Length"),
         zaxis = list(title = "Petal.Width")
         )); p3d

# Correlation Matrix
c <- as.matrix( cor(iris[1:4]) )
plot_ly(x=colnames(c),y=rownames(desc(c)),z=c,type='heatmap')

# Correlation Plot
library(corrplot)
corrplot(c, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

# Heatmap
data=as.matrix(iris)
plot_ly(x=colnames(data),
        y=rownames(data),
        z=data,type='heatmap')

# with normalization (right)
data=apply(data, 2, function(x){x/mean(x)})
plot_ly(x=colnames(data), y=rownames(data), z = data, type = "heatmap")

pn <- plot_ly(data = iris_norm[5:9], color = ~Species,
             x = ~Sepal.Length_norm, y = ~Petal.Length_norm)

