##############################
##MODELING MOSQUITOES##
#######################

# NOTES:
# ALL NOTES GO IN THIS AREA
# 1. do we want to use lagged weather variables because yesterday's weather affects today's hatch?
# 2. need to merge with nearest spray or perhaps merge all sprays and then see the effect area of the spray
#  3. need to include month
#  4. need to include past weather - mosquitos live 10 days
#       need to really get past wetness and past temperature (former being good for mosq. latter being bad for cold)
# 5. prolonged rains might be terrible. sprinkles might be better
# 6. oversample wnvpresent==1
# 7. where is the wind blowing in relation to lattitude/longitude and see if number of mosquitos in one area are going to move toward the other part of the city
#       -so if houston has 1000 mosquitos on january 1st and the direction is northwest austin might have a lot of mosquitos on jan 5th 
#       -need to get the speed of the wind and the result direction to get the exact lat long and the distance from 1000
#       -resultdir is possibly the 360 direction/10 off of true north
#       -potentially sample different percentages of the average speed of wind
#8. my task - to deal with wind and lagged variables


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

source(file.path(data_dir,'scripts/helperFunctions.R')) #pulling in helper functions
source(file.path(data_dir,'scripts/dependencies.R')) #reading in library dependencies


train<-fread(file.path(data_dir,'train.csv')) #i'd rather use data.tables


train[,Date:=as.Date(Date)]
train[,Species:=as.factor(Species)]
train[,Trap:=as.factor(Trap)]

weather<-fread(file.path(data_dir,'weather.csv'))

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

for(i in names(weather)[names(weather) %ni% 'Date']){
    print(i)
    a<-median(weather[,eval(as.name(i))],na.rm=TRUE)
    weather[!is.finite(weather[,eval(as.name(i))]),eval(as.name(i)):=a]
    weather[is.na(weather[,eval(as.name(i))]),eval(as.name(i)):=a]
    
    
}


    


train<-merge(train,weather,by='Date',all.x=TRUE)



# spray info --------------------------------------------------------------

#notes - need to merge based on location. only for 2011-2013
spray<-fread(file.path(data_dir,'spray.csv'))
spray[,Date:=as.Date(Date)]
spray[,combTime:=paste(Date,Time)]
spray[,combTime:=as.POSIXct(combTime,format='%Y-%m-%d %X')]






# basic info --------------------------------------------------------------

tabling<-function(dat,var){
    print(var)
    table(dat[,eval(as.name(var))])
    
}

#why are there more addresses than traps?
#delete<-train[,list(address=length(unique(Address))),by=Trap]
#T009, T035
#delete<-subset(train,subset=Trap %in% c('T009','T035'))
#delete<-delete[!duplicated(delete[,c('Trap','Address'),with=FALSE]),]
#within 4 kilometers of each other
vars<-c('Species','NumMosquitos','WnvPresent','Trap','AddressAccuracy')

lapply(vars,tabling,dat=train)
#notes:
# lot of different traps
# not a lot of west nile virus
# looks like data may be top coded at 50 for numMosq
# some species are very rare
# 3 5 8 and 9 for accuracy of address

# feature engineering -----------------------------------------------------

#doing lags on weather variables

#getting lagged dates so i can merge#
for(i in 1:7){
    
    train[,eval(as.name(paste0('Date',i))):=Date-i]
    
}

#changing names of weather dataset
init.names<-copy(colnames(weather))
for(i in 2:7){
    
    print(i)
    setnames(weather,colnames(weather),paste0(init.names,i)) #going to change all of the names so i can reference them as lags
    train<-merge(train,weather,by=paste0('Date',i),all.x=TRUE)
    
}
setnames(weather,colnames(weather),init.names) #returning names back


# modeling ----------------------------------------------------------------
dont.use<-c(names(train)[grep('Date',names(train))], 'Address','Street','AddressNumberAndStreet','Lattitude','Longitude','AddressAccuracy','WnvPresent','Trap')
vars<-names(train)[names(train) %ni% dont.use]

validation.dat<-x.validate(data=train,vtu=vars,outVar='WnvPresent',FUN=Random,err='auc',dat.return=TRUE)

x.validate(data=train,vtu=vars,outVar='WnvPresent',FUN=Random,err='auc')