library(shiny)
library(caret)
#Utilities function used for analysis
##wtr2: function used for calculating weighted R squre in the testing set
wtr2 = function(prediction, actual, weight){
    weight_term = weight/sum(weight)
    sse = sum(weight_term*(actual-prediction)^2)
    sstotal = sum(weight_term*(actual-sum(weight_term*actual))^2)
    return(1-sse/sstotal)
}
#pgreaterthan: A function to calculate the percentage of values that is greater than specified value in a numeric vector
pgreaterthan = function(numeric_vector, compVal){
    return(length(numeric_vector[numeric_vector>compVal])/length(numeric_vector))
}

#Data sets used
##Grocery Data
groceryData <- read.csv("data/Datasets/groceryr.csv")

# All the calculated fit parameters were already in this dataset
grocFit <- lm(formula = Source ~ LnWkrDen + LnWkHrs + LnSqft + WIFDens + CookDen + 
              HDDxpH + CDDxpC, weights=NewWt, data = groceryData)

##Office Data
officeData <- read.csv("data/Datasets/Office482.csv")

#calculate the fit parameters
LnArea  <- with(officeData, log(SQFT))
PcDens  <- with(officeData, 1000*PCNUM/SQFT)
ReFrDen <- with(officeData, 1000*(ReFridge+VnFridge)/SQFT)
CoFrDen <- with(officeData, 1000*(WiFridge+ClFridge+OpFridge)/SQFT)

HDDxpH <- with(officeData, HDD*HEATP/100)
CDDxpC <- with(officeData, CDD*COOLP/100)


LnWkHr  <- with(officeData, log(WKHRS))
LnWkerDen <- with(officeData, log(1000*NWKER/SQFT))

officeFit <- lm(formula = SourceEUI ~ LnArea + PcDens + LnWkHr + LnWkerDen + HDDxpH + CDDxpC + 
              Bank50:LnArea + Bank50:LnWkerDen + Bank50 + ReFrDen + CoFrDen, weights=ADJWT, data = officeData)


#A function design for doing cross-validation.

xvalidation <- function(fullfit, trainingPerc, weighted_predict = T){
    shrinkage = numeric(1000)
    testrsq = numeric(1000)
    trainrsq = numeric(1000)
    for(i in 1:1000){
        #groceryData <- read.csv("data/GrocRand.csv")
        data = fullfit$model
        data["weight_term"] = fullfit$weight
        inTrain = createDataPartition(data[,1], list = FALSE,p = trainingPerc) 
        training = data[inTrain,]
        test = data[-inTrain,]
        fitFormula = formula(fullfit)
        fit = lm(fitFormula, data = training, weight = weight_term)
        train_rsq = summary(fit)$r.squared
        test_predict = predict(fit, test)
        test_rsq = wtr2(test_predict, test[,1], test$weight_term)
        if(weighted_predict == F){
            test_rsq = cor(test_predict, test[,1])^2
        }
        shrinkage[i] = train_rsq - test_rsq
        testrsq[i] = test_rsq
        trainrsq[i] = train_rsq
        #print(paste("run",i,"Training rsq:",train_rsq,"Testing_rsq:",test_rsq))
    }
    return(list(training.rsq = trainrsq, test.rsq =  testrsq, shrinkage = shrinkage))
}


shinyServer(function(input,output){
    output$r2hist = renderPlot({
        if(input$DataSet ==1){
            info = xvalidation(officeFit,input$training_percent)
            par(mfrow = c(2,1))
            hist(info$training.rsq, main = "R square of training set", breaks = 20, xlab = "Training R squares")
            hist(info$test.rsq, main = "R square of testing set", breaks = 20,xlab = "Testing R squares")
        }
        else{
            info = xvalidation(grocFit,input$training_percent)
            par(mfrow = c(2,1))
            hist(info$training.rsq, main = "R square of training set", breaks = 20, xlab = "Training R squares")
            hist(info$test.rsq, main = "R square of testing set", breaks = 20,xlab = "Testing R squares")
        }
        
    })
   
    }

            )