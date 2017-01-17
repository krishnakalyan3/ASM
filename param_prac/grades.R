rm(list=ls())

setwd('/Users/krishna/Downloads/exam/param')
load('Grades.Rdata')
ls()
str(Grades)

attach(Grades)
plot(Grades$sat,Grades$gpa,pch=19, col='red')

# Linear Relation
model1 <- lm(gpa~sat,data=Grades)
summary(model1)
abline(model1, col='blue')


