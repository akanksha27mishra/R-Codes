
#Using RMongo

install.packages("devtools")
library(devtools)

install_github("mongosoup/rmongodb")
library(rmongodb)

install_github("tc/RMongo")
library("RMongo")
rmng<- mongoDbConnect("Covid_Analysis","localhost",27017)

out<-dbInsertDocument(rmng,"New_Patients",'{"patient_id":12345678,"birth_year":1999,
                      "region":"china","sex":"Male"}')
out<-dbInsertDocument(rmng,"New_Patients",'{"patient_id":9876,"birth_year":1989,
                      "region":"china","sex":"Male"}')
out

output<-dbGetQuery(rmng,"New_Patients",'{"patient_id":12345678,"birth_year":1999,
                      "region":"china","sex":"Male"}')

output

Alloutput1<-dbGetQuery(rmng,"New_Patients",'{}')

Alloutput1


#not working----mongo.update(rmng,New_Patients, list(patient_id="9876"), list(patient_id="9876", birth_year=1976))


#########################

# using Mongo Lite package

library(mongolite)
install.packages("mongolite")

setwd("/Users/akankshamishra/Documents/covid-19 analysis/correct data")
patients=data.table::fread("patient.csv")
names(patients)

my_collection<-mongo(collection = "patients",db="Covid_Analysis")
my_collection$insert(patients)
my_collection$count()
my_collection$iterate()$one()

length(my_collection$distinct("PrimaryType"))

query1<-my_collection$find('{"country":"China","sex":"male"}')
ncol(query1)
query1

###################
mtcars
#con<-mongo("mtcars",url="mongodb://readwrite:test@mongo.opencpu.org:43942/jeroen_test?retryWrites=false")
con<-mongo("mtcars",url="mongodb://localhost:27017/MongoDB_Test")


if(con$count()>0) con$drop()
con$insert(mtcars)
stopifnot(con$count()==nrow(mtcars))
#queryData

mydata<-con$find()
mydata
stopifnot(all.equal(mydata,mtcars))
#con$drop()

#Automatically disconnect when connection is removed

#rm(con)
#gc()

#dplyr example
library(nycflights13)

#Insert data

m<-mongo(collection = "nycfilghts")
m$drop()
m$insert(flights)

m$count('{"month":1,"day":1}')
jan1<-m$find('{"month":1,"day":1}')
jan1


#sorting

jan1sort<-m$find('{"month":1,"day":1}',sort='{"distance": +1}')
head(jan1sort)

m$index(add="distance")
allflights<-m$find(sort='{"distance":-1}')


#select limited columns
jan1<-m$find('{"month":1,"day":1}',fields = '{"_id":0,"distance":1,"carrier":1}')
jan1

#List Unique Values

m$distinct("carrier")
m$distinct("carrier",'{"distance":{"$gt":3000}}')

#Tabulate

m$aggregate('[{"$group":{"_id":"$carrier", "count": {"$sum":1}, "average":{"$avg":"$distance"}}}]')

#Map reduce binning

#The Map function takes an input pair and produces a set of intermediate key/value pairs. 
#The Reduce function accepts an intermediate key and a set of values for that key (possibly grouped by the MapReduce library). 
#It merges these values together to form a possibly smaller set of values

mr<-m$mapreduce(map="function(){emit(Math.floor(this.distance/100)*100,1)}",
                  reduce = "function(id,counts){return Array.sum(counts)}")
mr





