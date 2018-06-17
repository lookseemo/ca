########### Sample R script for Customer Churn ###############
## Source: IBM Developerworks

library(ggplot2)


## Code below creates a data frame called mydata based on an input file
mydata <- read.csv("customer_data.csv")

## Code below performs statistical analysis on the data frame
mydata$IN_B2B_INDUSTRY <- factor(mydata$IN_B2B_INDUSTRY)
mydata$TOTAL_BUY <- factor(mydata$TOTAL_BUY)

## Code below performs Logistic Regression
mylogit <- glm(CENSOR ~ AVG_SPENT_RETAIN_PM + AVG_SQ_SPENT_RETAIN_PM + TOTAL_BUY + TOTAL_BUY_FREQ 
              + IN_B2B_INDUSTRY + ANNUAL_REVENUE_MIL + TOTAL_EMPLOYEES, data = mydata, family = binomial)
summary(mylogit)
newdata2 <- with(mydata, data.frame(DURATION = rep(seq(from = 36, to = 730, length.out = 100)),
                                   AVG_SPENT_RETAIN_PM = rep(seq(from = 0, to = 145, length.out = 100)),
                                   AVG_SQ_SPENT_RETAIN_PM = rep(seq(from = 0, to = 21071, length.out = 100)),
                                   IN_B2B_INDUSTRY = factor(rep(0:1, each = 100)), 
                                   ANNUAL_REVENUE_MIL = rep(seq(from = 1, to = 77, length.out = 100)),
                                   TOTAL_EMPLOYEES = rep(seq(from = 0, to = 2000, length.out = 100)),
                                   TOTAL_BUY = factor(rep(1:6, each = 100)),
                                   TOTAL_BUY_FREQ = rep(seq(from = 1, to = 21, length.out = 100))))
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                   se = TRUE))
newdata3 <- within(newdata3, {churnprob <- plogis(fit)
                             LL <- plogis(fit - (1.96 * se.fit))
                             UL <- plogis(fit + (1.96 * se.fit))})
cutpoints <- quantile(newdata3$DURATION, seq(0, 1, length = 4), na.rm = TRUE)
newdata3$dur_dec <- cut(newdata3$DURATION, cutpoints)
levels(newdata3$dur_dec)#
newdata3$IN_B2B_INDUSTRY <- factor(newdata3$IN_B2B_INDUSTRY, labels = c("B2C", "B2B"))
newdata3$dur_dec <- factor(newdata3$dur_dec, labels = c("Duration:36-267", "Duration:267-499", "Duration:499-730"))

## Code below saves the output in CSV format
write.csv(newdata3, file = "churn.csv")

## Code below generates a plot
churn <- ggplot(newdata3, aes(x = AVG_SPENT_RETAIN_PM, y = churnprob)) + 
 geom_point(aes(color = TOTAL_BUY_FREQ),size = 1, alpha = 0.75) +
 scale_colour_gradient(low="green", high = "red") + 
 geom_smooth(method = "lm", se=FALSE,  size=2, col="steelblue", linetype=3) + 
 facet_wrap(dur_dec~IN_B2B_INDUSTRY, nrow=4,ncol=2)+
 labs(title="Customer Churn")+   labs(x="Average Retaining Expense",y="Customer churn Probability")
ggsave(filename = "customer-img.jpg", plot = churn, height=3, width=3, scale=2, dpi=200)
## Code below saves the plot as PDF
ggsave(filename = "churn.pdf", height=7, width=7)
