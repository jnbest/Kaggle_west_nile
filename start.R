##############################
##MODELING MOSQUITOES##
#######################

# NOTES:
# ALL NOTES GO IN THIS AREA
# 1. do we want to use lagged weather variables because yesterday's weather affects today's hatch?
# 2. need to merge with nearest spray or perhaps merge all sprays and then see the effect area of the spray
#  
#  
# 
# 
# 
# 
# 
# 


# working with train data -------------------------------------------------------

data_dir <- "C:/Users/jbest/Box Sync/Personal (Private) - jbest/R/kaggle/westNile"
source(file.path(data_dir,'scripts/helperFunctions.R')) #pulling in helper functions
source(file.path(data_dir,'scripts/dependencies.R')) #reading in library dependencies


train<-fread(file.path(data_dir,'train.csv')) #i'd rather use data.tables
weather<-fread(file.path(data_dir,'weather.csv'))

train[,Date:=as.Date(Date)]
train[,Species:=as.factor(Species)]
train[,Trap:=as.factor(Trap)]



# working with weather ----------------------------------------------------
#there are 2 stations and they report different numbers. sometimes station 2 has missing observations as well
#this part could be made more sophisticated but doing it this way for now.
weather[,Date:=as.Date(Date)]

#extracting weather conditions from Codesum
conditions<-c('FC','TS','RA','GR','RA','DZ','SN','SG','PL','IC','FG','BR',
              'UP','HZ','FU','VA','DU','DS','PO','SA','SS','PY','SQ','DR','SH',
              'FZ','MI','PR','BC','VC','BL','+','-')

conditioning<-function(con){
    
    weather[grep(con,weather$CodeSum),eval(as.name(con)):=1] #finding each instance of different conditions
    
    if(con %in% names(weather)) weather[is.na(weather[,eval(as.name(con))]),eval(as.name(con)):=0] #replacing NAs with 0s
    
}

for(i in conditions){ #running the function for each potential condition
    conditioning(i)
}


#collapsing the 2 stations into 1 by means/maxes and excluding missing values

key.vars1<-c('Tmax','Tmin','Tavg','Depart','DewPoint','WetBulb','Heat','Cool','Sunrise','Sunset','PrecipTotal','StnPressure',
             'SeaLevel','ResultSpeed','ResultDir','AvgSpeed')
key.vars2<-c('TS','RA','GR','DZ','SN','FG','BR','HZ','FU','SQ','MI','BC','VC')

#first making key.vars1 numeric

weather[,(key.vars1):=lapply(.SD, as.numeric), .SDcols=key.vars1]

#next getting mean of key.vars1 and max of key.vars2
weather1 <- weather[, c(lapply(.SD,mean,na.rm=TRUE)), by=Date, .SDcols=key.vars1]

weather2 <- weather[, c(lapply(.SD,max,na.rm=TRUE)), by=Date, .SDcols=key.vars2]
#merging back together

weather<-merge(weather1,weather2,by='Date')

train<-merge(train,weather,by='Date',all.x=TRUE)



# spray info --------------------------------------------------------------

#notes - need to merge based on location. only for 2011-2013
spray<-fread(file.path(data_dir,'spray.csv'))
spray[,Date:=as.Date(Date)]
spray[,combTime:=paste(Date,Time)]
spray[,combTime:=as.POSIXct(combTime,format='%Y-%m-%d %X')]






#train<-merge(train,spray,by='Date')
# basic info --------------------------------------------------------------

tabling<-function(dat,var){
    print(var)
    table(dat[,eval(as.name(var))])
    
}

vars<-c('Species','NumMosquitos','WnvPresent','Trap','AddressAccuracy')

lapply(vars,tabling,dat=train)
#notes:
# lot of different traps
# not a lot of west nile virus
# looks like data may be top coded at 50 for numMosq
# some species are very rare
# 3 5 8 and 9 for accuracy of address

# feature engineering -----------------------------------------------------




