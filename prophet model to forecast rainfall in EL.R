library(prophet)
library(readxl)
data=rainfall
head(data)
colnames(data)=c("ds","y")
m=prophet(data,weekly.seasonality = FALSE,
          daily.seasonality = FALSE)
future=make_future_dataframe(m,periods = 50)
forecast=predict(m,future)
