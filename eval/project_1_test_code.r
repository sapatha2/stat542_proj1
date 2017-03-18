############################################################################
# Put all files in the same folder                                     #
# In the R console, use setwd() to change the working directory.           #
############################################################################

start.time = Sys.time()
source('mymain.R')
end.time = Sys.time()
run.time = as.numeric(difftime(end.time, start.time, units = 'sec'))

# calculate the test error on the test set
test = read.csv('test.csv')

temp.err = rep(NA, 3)

for (met in 1:3){

    pred = read.csv(paste('mysubmission', met, '.txt', sep = ''), sep = ',')
    yhat = log(pred$SalePrice)
    test = read.csv('test_label.csv', sep = ',')
    y = log(test$SalePrice)
    temp.err[met] = sqrt(mean((y - yhat)^2))

}
print(temp.err)
print(run.time)
