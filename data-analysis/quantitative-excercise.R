corr
data <- file[1:1000, c(6, 7,8, 17)]

# Examining data
par(mfrow=c(3,2))
plot.design(data)
plot.design(data,fun=median)
plot(bug ~ la + ld, data=data)

interaction.plot(data$la, + data$ld, data$entropy) 

dat <- aov(bug~la*ld, data) 
summary(dat) 

summary(file)

cor(data$la, data$ld)

num_values<- file[, c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
m <- rcorr(as.matrix(num_values))

# Correlation graph
corrgram(num_values)

# Principal component analysis
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
principal_values<- file[, c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
prin_comp <- prcomp(principal_values, scale. = T)
# names(prin_comp)
# prin_comp$rotation

std_dev <- prin_comp$sdev
pr_var <- std_dev^2 # Variance

pr_var

prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

tot <- 0;
vec <- 0;
for (var in prop_varex) {
  vec <- vec + 1
  tot <- tot + var
  print(paste(vec, ": ",tot))
}


abline(0.9581,0, col="red")
text(1,0.958, "0.958", col = 2, adj = c(-.1, -.1))


# Exploratory data analytics
plot(ecdf(num_values$lt))
plot(ecdf(num_values$lt), add=TRUE, pch=9)

par(mfrow=c(2,2))
boxplot(num_values$la, title="la")
boxplot(num_values$lt)

# Logistic regression model
# This is instead of a linear regression
train_data <- file[, c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
model <- rms::lrm(formula = as.factor(bug) ~ ns + nm + nf + entropy + la + ld + lt + fix + ndev + pd + 
                    npt + exp + rexp + sexp, data = train_data, y=TRUE, x=TRUE)
summary(model)
par(oma=c(0,0,5,0))
par(mfrow=c(1,2))
hist(resid(model), main ="Histrogram", xlab ="Residuals", ylab = "Frequency")
qqnorm(resid(model))
options(prType="latex")
print(model, digits=4, strata.coefs=FALSE, coefs=TRUE, title='Logistic Regression Model')

# Boxplots
par(mfrow=c(1,1))
box_data <- file[, c(3,4,5,6,7,8,10,11,13)]
box_data$ld[box_data$ld > 1000] <- 0.001
box_data[box_data==0]<-NA
boxplot(box_data, log='y', ylab="Value   (log-scale)")

par(mfrow=c(1,1))
box_data <- file[, c(9,12,14,15,16)]
box_data$ld[box_data$ld > 1000] <- 0.001
box_data[box_data==0]<-NA
boxplot(box_data, log='y', ylab="Value   (log-scale)")

par(mfrow=c(1,1))
box_data <- file[, c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
box_data$ld[box_data$ld > 1000] <- 0.001
box_data[box_data==0]<-NA
boxplot(box_data, log='y', ylab="Value   (log-scale)")

# Power analysis
categories <- 2
pwr.t.test(n=NULL, d=0.1, power=0.9)
pwr.chisq.test(N=NULL, w = 0.2, power = 0.9, sig.level = 0.05, df = categories - 1)
