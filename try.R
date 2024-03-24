library(ggplot2)
library(dplyr)
data=data.frame(
  day=as.Date("2017-06-14") - 0:364,
  value=runif(365) + seq(-140, 224)^2/10000
)
ggplot(data,aes(x=day,y=value))+
  geom_line()
