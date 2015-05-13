# working with train data -------------------------------------------------------

jason_dir <- "C:/Users/jbest/Box Sync/Personal (Private) - jbest/R/kaggle/westNile"

#Brian's Directory
brian_dir <- "C:/Users/user1/Documents/Kaggle"

if (file.exists(brian_dir)){
    data_dir<-brian_dir
} else if (file.exists(jason_dir)) {
    
    data_dir<-jason_dir
} else {
    
    stop('directory doesnt exist')
}
