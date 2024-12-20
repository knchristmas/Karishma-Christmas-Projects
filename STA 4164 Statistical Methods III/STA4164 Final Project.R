#scatterplots
plot(socialmedia$RL_Mot_Friendship, socialmedia$ConnectionScore)
plot(socialmedia$RL_Mot_Connection, socialmedia$ConnectionScore)
plot(socialmedia$RL_Mot_Info, socialmedia$ConnectionScore)
plot(socialmedia$Age, socialmedia$ConnectionScore)

#summary statistics
socialmedia<- (Motivational_Underpinnings_of_Social_Media_Use_Dataset)
summary(socialmedia$Sex)
summary(socialmedia$SNS_Total)
summary(socialmedia$RL_Mot_Connection)
summary(socialmedia$ConnectionScore) 
summary(socialmedia$Age) 
summary(socialmedia$RL_Mot_Info) 
summary(socialmedia$RL_Mot_Friendship)
#sex proportions
prop.table(table(socialmedia$Sex, exclude = NULL))


#boxplots for the different motivation types
boxplot(RL_Mot_Info, RL_Mot_Friendship, RL_Mot_Connection, names = c("Info", "Friendship", "Connection"), xlab = "Motivation Type")
boxplot(male$ConnectionScore, female$ConnectionScore, names = c("male", "female"), ylab = "ConnectionScore")

#correlation matrix that works because removes NAs
cor(socialmediadf, use = "complete.obs")

#interaction age:total social media use
interactiontest<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$SNS_Total + socialmedia$Age*socialmedia$SNS_Total, data = socialmedia)
summary(interactiontest) 
interactionmodel8<- lm(socialmedia$ConnectionScore ~ socialmedia$SNS_Total + socialmedia$Age + socialmedia$Sex + I(log(socialmedia$RL_Mot_Friendship)) + socialmedia$Age*socialmedia$SNS_Total, data = socialmedia)
summary(interactionmodel8)
#no interaction :( womp womp

#cooks distance
print(sort(cooks.distance(transformmodel8)))

#jackknife
print(sort(studres(transformmodel8)))
t <- qt(.025, 356-16-2, lower.tail = FALSE)
print(t)
[1] 1.967007
#outliers: 296, 218, 99, 30, 253, 238, 256, 150, 156, 135, 129, 226, 170, 341, 281, 69, 112, 267, 56

#leverage
#2(k+1)/n = 2*16/356 = 0.08988764
print(sort(hatvalues(transformmodel8)))
#outliers: 255, 224

#shapiro normality doesnt work 
shapiro.test(transformmodel8)

#initial fullmodel
model<- lm(socialmedia$ConnectionScore ~ socialmedia$Sex + socialmedia$SNS_Total + socialmedia$RL_Mot_Connection + socialmedia$Age + socialmedia$RL_Mot_Info + socialmedia$RL_Mot_Friendship, data = socialmedia)


#transformations
#model we went with :)
transformmodel8<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + I(log(socialmedia$RL_Mot_Friendship)), data = socialmedia)
plot(transfrommodel8)
summary(transformmodel8)
#no transformations
model8<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Friendship, data = socialmedia)
plot(model8)
#x transformations
#squared
model8x2<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + I(socialmedia$RL_Mot_Friendship^2), data = socialmedia)
plot(model8x2)
#sqrt
model8sqrtx<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + I(sqrt(socialmedia$RL_Mot_Friendship)), data = socialmedia)
plot(model8sqrtx)
#cubed
model8x3<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + I(socialmedia$RL_Mot_Friendship^3), data = socialmedia)
plot(model8x3)
#1/x
model81x<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + I(1/socialmedia$RL_Mot_Friendship), data = socialmedia)
plot(model81x)
#1/x^2
model81x2<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + I((1/socialmedia$RL_Mot_Friendship^2)), data = socialmedia)
plot(model81x2)
#y transformations
#squared
model8y2<- lm(I(socialmedia$ConnectionScore^2) ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Friendship, data = socialmedia)
plot(model8y2)
#sqrt
model8sqrty<- lm(I(sqrt(socialmedia$ConnectionScore)) ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Friendship, data = socialmedia)
plot(model8sqrty)
#cubed
model8y3<- lm(I(socialmedia$ConnectionScore^3) ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Friendship, data = socialmedia)
plot(model8y3)
#1/y
model81y<- lm(I(1/socialmedia$ConnectionScore) ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Friendship, data = socialmedia)
plot(model81y)
#1/y^2
model81y2<- lm(I(1/socialmedia$ConnectionScore^2) ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Friendship, data = socialmedia)
plot(model81y2)

#motivation models
model7<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Info, data = clean)
summary(model7)
plot(model7)
model8<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Friendship, data = socialmedia)
summary(model8)
plot(model8)
model9<- lm(socialmedia$ConnectionScore ~ socialmedia$Age + socialmedia$Sex + socialmedia$RL_Mot_Connection, data = socialmedia)
summary(model9)
plot(model9)

#simple linear regression models of all of the IVs
model1<-(lm(socialmedia$ConnectionScore ~ socialmedia$Sex, data = socialmedia))
summary(model1)

model2<-(lm(socialmedia$ConnectionScore ~ socialmedia$SNS_Total, data = socialmedia))
summary(model2)

model3<-lm(socialmedia$ConnectionScore ~ socialmedia$RL_Mot_Connection, data = socialmedia)
summary(model3)

model4<-lm(socialmedia$ConnectionScore ~ socialmedia$RL_Mot_Info, data = socialmedia)
summary(model4)

model5<-lm(socialmedia$ConnectionScore ~ socialmedia$RL_Mot_Friendship, data = socialmedia)
summary(model5)

model6<-lm(socialmedia$ConnectionScore ~ socialmedia$Age, data = socialmedia)
summary(model6)


#overall F test
summary(transformmodel8)
#not significant

#AIC scores
AIC(model9)
[1] 2000.036
AIC(model8)
[1] 1993.609
AIC(model7)
[1] 1998.552
#model 8 (friendship motivation) is the best

