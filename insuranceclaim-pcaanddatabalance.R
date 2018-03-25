mydata<-read.csv(file.choose())
summary(mydata)
names(mydata)
ind=mydata[,2:59]
scaleddata=scale(ind)
prin_comp=prcomp(ind,scale. = T)
summary(prin_comp)
str(prin_comp)
prin_comp$rotation
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

summary(prin_comp)
str(prin_comp)
prin_comp$x
modeldata=data.frame(prin_comp$x,mydata$target)
str(prin_comp)
prin_comp$rotation

#Checking data balance
table(mydata$target)

prop.table(table(mydata$target))
nrow(mydata)

library(ROSE)

data_balanced_over <- ovun.sample(target ~ ., data = mydata, method = "over",N = 595212)$data

library(ROSE)

data_balanced_under <- ovun.sample(target ~ ., data = mydata, method = "under",N = 595212)$data

data_both= ROSE(target~., data = mydata, seed = 1)$data

