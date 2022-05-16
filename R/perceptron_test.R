require(ggplot2)
require(data.table)
require(compiler)
source('~/R/generate_data.R')
source('~/R/perceptron.R')

lsData <- linearSepData(n=100)
DT <- lsData$data
perc <- perceptron(DT)

# Plot
ggplot(data=DT, aes(x=x, y=y, color=as.factor(cls))) + 
  geom_point() + 
  geom_abline(slope = lsData$slope, intercept = lsData$intercept, color='red') +
  geom_abline(slope = perc$slope, intercept = perc$intercept, color='green')