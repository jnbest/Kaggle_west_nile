# helper functions --------------------------------------------------------


initialized<-c('%ni%','jason','jasonRm','Cube','Random','Line','Neural','SVM','x.validate','initialized','var.Select','Logit','createDummies','s','h','n',
               'read.csv2','write.csv2','write.delim','read.delim3','lowerNames')




`%ni%`<-Negate(`%in%`)

s <- base::summary
h <- utils::head
n <- base::names

createDummies <- function(x, df, keepNAs = TRUE) {
    for (i in seq(1, length(unique(df[, x])))) {
        if(keepNAs) {
            df[, paste(x,".", i, sep = "")] <- ifelse(df[, x] != i, 0, 1)
        } else {
            df[, paste(x,".", i, sep = "")] <- ifelse(df[, x] != i | is.na(df[, x]) , 0, 1)     
        }
    }
    df
}

jason<-function(lib){ #helper for initializing packages
    if(lib %in% rownames(installed.packages()) == FALSE) {
        print('not installed. now installing')
        install.packages(lib)}
    #print(lib)
    require(lib,character.only = TRUE)
}

jasonRm<-function(){
    
    a<-ls(name=.GlobalEnv)[ls(name=.GlobalEnv) %ni% initialized] 
    rm(list=a,envir=.GlobalEnv)
    
}


Cube<-function (train_dat, test_dat, variables, y_var, committees = 20) {
    
    if('data.table' %in% class(train_dat)){
        x <- train_dat[, names(train_dat)[names(train_dat) %in% variables],with=FALSE]
        print(length(x))
        print(nrow(x))
        committeeModel <- cubist(x = x, y = train_dat[, eval(as.name(y_var))], committees = committees)
    } else {
        x <- train_dat[, names(train_dat)[names(train_dat) %in% variables]]
        committeeModel <- cubist(x = x, y = train_dat[, y_var], committees = committees) 
    }
    pred <- predict(committeeModel, newdata = test_dat)
    return(pred)
}

Line<-function(train_dat,test_dat,variables,y_var){
    fm <- as.formula(paste(y_var," ~", paste(variables, collapse = "+")))
    
    glm1<-glm(fm,data=train_dat)
    pred<-predict(glm1,newdata=test_dat)
    return(pred)
}

#EDITS:#
#trace("avNNet.default",edit=TRUE)
#i edited the nnet call to take out 'trace'


Neural<-function(train_dat,test_dat,variables,y_var) {
    fm<-as.formula(paste(y_var,'~',paste(variables,collapse='+')))
    
    fit  <- avNNet(fm, data=train_dat, repeats=25, size=3, decay=0.1,
                   linout=TRUE)
    
    pred<-predict(fit,newdata=test_dat)
    
}

Random<-function (train_dat, test_dat, variables, y_var) 
{
    x<-train_dat[,names(train_dat) %in% variables]
    fgl.res <- tuneRF(x, train_dat[,y_var], stepFactor=1.5)
    mtry<-fgl.res[fgl.res[,'OOBError']==min(fgl.res[,'OOBError']),'mtry']
    
    fm <- as.formula(paste(y_var, "~", paste(variables, collapse = "+")))
    fit <- randomForest(fm, data = train_dat, ntree = 500,mtry=mtry)
    pred <- predict(fit, newdata = test_dat)
}

SVM<-function(train_dat,test_dat,variables,y_var) {
    fm<-as.formula(paste(y_var,'~',paste(variables,collapse='+')))
    
    fit  <- svm(fm, data=train_dat, ntree=500)
    
    pred<-predict(fit,newdata=test_dat)
    
}

x.validate<-function (data, vtu, outVar, FUN = Cube, k = 5, err = "perc",var.select = FALSE, var.print = FALSE, plotvar, dat.return = FALSE) {
    k = k
    err.vect = rep(NA, k)
    print(paste("number of validations = ", k))
    holder <- matrix(nrow = nrow(data), ncol = 2)
    for (i in 1:k) {
        indexes = sample(2, length(data), replace = TRUE, prob = c(0.3, 
                                                                   0.7))
        train <- data[indexes == 2, ]
        test <- data[indexes == 1, ]
        if (var.select == TRUE) {
            vtu1 <- var.Select(data = train, y.var = outVar, 
                               variables = vtu)
            if (var.print == TRUE) {
                print("variable list =")
                print(vtu1)
            }
        }
        else {
            vtu1 = vtu
        }
        test$pred <- FUN(train_dat = train, test_dat = test, 
                         variables = vtu1, y_var = outVar)
        if (err == "mse") {
            sim <- as.matrix(test$pred)
            if ("data.table" %in% class(test)) {
                obs <- as.matrix(test[, eval(outVar), with = FALSE])
            }
            else {
                obs <- as.matrix(test[, outVar])
            }
            err.vect[i] = mse(sim, obs)
        }
        else if (err == "perc") {
            if ("data.table" %in% class(test)) {
                tot.err = sum(abs(test$pred - test[, eval(outVar), 
                                                   with = FALSE]))
                perc.err = 1 - tot.err/sum(test[, eval(outVar), 
                                                with = FALSE])
            }
            else {
                tot.err = sum(abs(test$pred - test[, outVar]))
                perc.err = 1 - tot.err/sum(test[, outVar])
            }
            err.vect[i] = perc.err
        } else if (err=='auc'){
            if ("data.table" %in% class(test)) {
                err.vect[i]=auc(test[,eval(outVar)],test$pred)
                
            }
            else {
                err.vect[i]=auc(test[,outVar],test$pred)
            }
            # err.vect[i] = perc.err
        }
        else if(err=='fact'){
            err.vect[i]=nrow(test[test$pred==test[,outVar],])/nrow(test)
        }
        else {
            print("error not correctly specified")
            break
        } 
        print(paste0("accuracy of fold ", i, " is ", round(err.vect[i] * 
                                                               100, 2), "%"))
        if (i == 1) {
            args <- as.list(match.call())
            if ("plotvar" %in% names(args)) {
                print("plotting")
                plot(test[, plotvar], (test[, outVar] - test$pred), 
                     ylab = "Residuals", xlab = plotvar, main = paste(outVar, 
                                                                      "Residuals Graph"))
            }
        }
    }
    print(paste0("average accuracy = ", round(mean(err.vect), 
                                              2) * 100, "%"))
    if (dat.return == TRUE) 
        return(test)
}

var.Select<-function(data,y.var,variables){
    #FORMULA WITH FEATURES#
    fm <- as.formula(paste(y.var, "~", paste(variables, collapse = "+")))
    x<-model.matrix(fm,data=data)
    x=x[,-1]
    
    #running the lasso algorithm#
    if('data.table' %in% class(data)){
        glmnet1<-cv.glmnet(x=x,y=data[,eval(as.name(y.var))],type.measure='mse',nfolds=5,alpha=.5)
        print('here1')
    } else {
        glmnet1<-cv.glmnet(x=x,y=data[,y.var],type.measure='mse',nfolds=5,alpha=.5)
    }
    
    #getting the coefficients of the minimized mean squared error#
    c<-coef(glmnet1,s='lambda.min',exact=TRUE)
    inds<-which(c!=0)
    #pulling out the associated variables#
    
    variables<-row.names(c)[inds]
    variables<-variables[variables %ni% '(Intercept)']
    return(variables)
    
}

Logit<-function (train_dat, test_dat, variables, y_var) 
{
    fm <- as.formula(paste(y_var, " ~", paste(variables, collapse = "+")))
    glm1 <- glm(fm, data = train_dat,family='binomial')
    pred <- predict(glm1, newdata = test_dat)
    return(pred)
}


read.csv2 <- function(...) read.csv(stringsAsFactors = FALSE, ...)
read.delim3 <- function(...) read.delim(stringsAsFactors = FALSE, ...)

write.csv2 <- function(...) write.csv(row.names = FALSE, ...)
write.delim <- function(...) write.table(row.names = FALSE, sep = "\t", ...)

lowerNames <- function(dat) structure(dat, names = tolower(names(dat)))
