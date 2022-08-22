data(tips, package = 'reshape2')
library(ggplot2)
qplot(total_bill, tip, data=tips, geom = "point")

myplot <- ggplot(tips, aes(x = total_bill, y = tip)) +
  geom_point(aes(color = sex)) + 
  geom_smooth(method = 'lm')

layer_point <- geom_point(
  mapping = aes(x = total_bill, y = tip, color = sex),
  data = tips,
  size = 3
)

model <- lm(tip ~ total_bill, data = tips)
fitted_tips <- data.frame(
  total_bill = tips$total_bill,
  predict(model, interval = "confidence")
)

head(fitted_tips)

layer_line <- geom_smooth(tip ~ total_bill, method='lm')



ggplot() + layer_point + layer_line
?geom_ribbon
