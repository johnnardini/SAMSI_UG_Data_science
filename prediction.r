#set our working directory
setwd("~/s19/SAMSI/NCCU/")

#read in student data
data <- read.csv("student.csv",header=TRUE)

#print column names
colnames(data)

# plot student grades against hours studying
plot(data$studytime,data$grade,xlab="Hours Spent Studying",ylab="Grade")

# plot student grades against number of absences
plot(data$absences,data$grade,xlab="Number of Absences",ylab="Grade")

### linear models

# fit a linear model to hours studying
# i.e., y = grade, x = study_time
model_hours <- lm(grade ~ studytime,data=data)
summary(model_hours)

# fit a linear model to absences
# i.e., y = grade, x = absences
model_absences <- lm(grade ~ absences,data=data)
summary(model_absences)

# fit a linear model to both absences and hours studying
# i.e., y = grade, x_1 = absences, x_2 = hours spent studying
model_absences_hours <- lm(grade ~ absences + studytime,data=data)
summary(model_absences_hours)

### predicting

#split data into males and females
males = subset(data,gender=='M')
females = subset(data,gender=='F')

#fit a linear model (grades = beta_0+beta_1*studytime) using the male data
model_hours_males <- lm(grade ~ studytime,data=males)
summary(model_hours_males)

#we will predict grades for mesh grid of study hours
hours <- data.frame(studytime=seq(1,4,.1))
#predict grades based on study hours
predict_model = predict(model_hours_males,hours)

#plot the predictions (line) against data for females (dots)
plot(females$studytime,females$grade,xlab="Hours studying",ylab="Grade",main = "Predicting female grades")
lines(hours$studytime,predict_model)
