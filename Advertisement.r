library(Metrics)
library(DAAG)
library(leaps)

plotError <- function(dataX,dataY,xlabel,ylabel,model,title,predicted_y)
{
    plot(x=dataX, y=dataY, xlab = xlabel,ylab = ylabel,
       main = title, abline(model))
    count = 1
    for (x in dataX)
    {
        X = c(x,x)
        Sales = c(dataY[count],predicted_y[count])
        lines(x = X, y = Sales, type = "l",col="red")
        count = count + 1
    }
}
adv_data = read.csv("Advertising.csv",sep=',')

#Splitting data into training(70%) and testing(30%)
train_dataset = adv_data[1:140,]
test_dataset = adv_data[141:200,]

#Linear regression
m_tv = lm(Sales ~ TV, data = train_dataset)
m_radio = lm(Sales ~ Radio, data = train_dataset)
m_np = lm(Sales ~ Newspaper, data = train_dataset)

#Plotting training and testing MSE
MSE_train_TV = mse(train_dataset$Sales,predict(m_tv, newdata = train_dataset))
MSE_train_RADIO = mse(train_dataset$Sales,predict(m_radio, newdata = train_dataset))
MSE_train_NP = mse(train_dataset$Sales,predict(m_np, newdata = train_dataset))

MSE_test_TV = mse(test_dataset$Sales,predict(m_tv, newdata = test_dataset))
MSE_test_RADIO = mse(test_dataset$Sales,predict(m_radio, newdata = test_dataset))
MSE_test_NP = mse(test_dataset$Sales,predict(m_np, newdata = test_dataset))

MSE_train <- c(MSE_train_TV, MSE_train_RADIO,MSE_train_NP)
MSE_test <- c(MSE_test_TV, MSE_test_RADIO,MSE_test_NP)
MSE <- rbind(MSE_train,MSE_test)
colnames(MSE) = c("TV", "Radio", "Newspaper")
barplot(MSE, main = "MSE", ylab = "MSE",beside = TRUE,
        col=c("green","blue"),ylim=c(0,40),legend.text=c('Training','Testing'))

#plotting training data errors
plotError(train_dataset$TV,train_dataset$Sales,xlabel="TV","Sales",
          m_tv,"Training Dataset Errors",predict(m_tv, newdata = train_dataset))

#plotting testing data errors
plotError(test_dataset$TV,test_dataset$Sales,xlabel="TV","Sales",
          m_tv,"Testing Dataset Errors",predict(m_tv, newdata = test_dataset))

#Cross validation
a = cv.lm(train_dataset, m_tv, m=3, plotit=TRUE, printit=TRUE)

#Subset selection
train_dataset = train_dataset[-1]
pairs(train_dataset)
regsub = regsubsets(Sales~.,train_dataset)
reg.summary = summary(regsub)
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
reg.summary$rss
reg.summary
