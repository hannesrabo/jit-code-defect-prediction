file = read.csv('C:\\Users\\Hannes\\Documents\\scientific-writing\\postgres.csv')

input = file
input <- file[, c(17,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
# input$bug <- as.logical(input$bug)
form <- as.formula(input)

observations <- nrow(input)
train_size <- round(observations * 0.7)
test_size <- observations - train_size

train_data <- input[1:train_size,]
test_data <- input[(train_size+1):observations,]
test_data_res <- test_data$bug
test_data$bug <- NULL

model <- rms::lrm(formula = as.factor(bug) ~ ns + nm + nf + entropy + la + ld + lt + fix + ndev + pd + 
                    npt + exp + rexp + sexp, data = train_data, x=TRUE)

L <- predict(model, se.fit=TRUE)
plogis(with(L, linear.predictors + 1.96*cbind(-se.fit,se.fit)))

fit <- predict(model, test_data, type="fitted")        # Prob(Y>=j) for new observation
fit_ind <- predict(model, test_data, type="fitted.ind")    # Prob(Y=j)
predict(model, test_data, type='mean', codes=TRUE) # predicts mean(y) using codes 1,2,3
m <- Mean(model, codes=TRUE)

pred <- predict(model, test_data)
pred2 <- test_data_res

m(pred)
