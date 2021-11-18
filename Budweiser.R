library(readxl)
quarterly <- read_excel("abinbev_quarterly.xlsx")
yearly <- read_excel("abinbev_yearly.xlsx")
market_share <- read_excel("abinbev_market_share.xlsx")




plot(revenue~sales_mktg_other, data=quarterly)
model1 <- lm(revenue~sales_mktg_other, data=quarterly)
summary(model1)

plot(revenue~sales_mktg_other_quarter_lag1, data=quarterly)
model2 <- lm(revenue~sales_mktg_other_quarter_lag1, data=quarterly)
summary(model2)

plot(revenue~sales_mktg_other_quarter_lag2, data=quarterly)
model3 <- lm(revenue~sales_mktg_other_quarter_lag2, data=quarterly)
summary(model3)

plot(revenue~advertising, data=yearly)
model4 <- lm(revenue~advertising, data=yearly)
summary(model4)

plot(revenue~advertising_year_lag1, data=yearly)
model5 <- lm(revenue~advertising_year_lag1, data=yearly)
summary(model5)




model6 <- lm(revenue~sales_mktg_other+year+quarter, data=quarterly)
summary(model6)

model7 <- lm(revenue~sales_mktg_other_quarter_lag1+year+quarter, data=quarterly)
summary(model7)

model8 <- lm(revenue~sales_mktg_other_quarter_lag2+year+quarter, data=quarterly)
summary(model8)

model9 <- lm(revenue~advertising+year, data=yearly)
summary(model9)

model10 <- lm(revenue~advertising_year_lag1+year, data=yearly)
summary(model10)




model11 <- lm(log(revenue)~log(sales_mktg_other), data=quarterly)
summary(model11)

model12 <- lm(log(revenue)~log(sales_mktg_other_quarter_lag1), data=quarterly)
summary(model12)

model13 <- lm(log(revenue)~log(sales_mktg_other_quarter_lag2), data=quarterly)
summary(model13)

model14 <- lm(log(revenue)~log(advertising), data=yearly)
summary(model14)

model15 <- lm(log(revenue)~log(advertising_year_lag1), data=yearly)
summary(model15)




model16 <- lm(log(revenue)~log(sales_mktg_other)+year+quarter, data=quarterly)
summary(model16)

model17 <- lm(log(revenue)~log(sales_mktg_other_quarter_lag1)+year+quarter, data=quarterly)
summary(model17)

model18 <- lm(log(revenue)~log(sales_mktg_other_quarter_lag2)+year+quarter, data=quarterly)
summary(model18)

model19 <- lm(log(revenue)~log(advertising)+year, data=yearly)
summary(model19)

model20 <- lm(log(revenue)~advertising_year_lag1+year, data=yearly)
summary(model20)




model21 <- lm(log(share)~log(advertising), data=market_share)
summary(model21)
model22 <- lm(log(share)~log(advertising)+year, data=market_share)
summary(model22)
