
# Read files into R - have to be in working directory!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#Subset for Baltimore and then for year 1999 and 2008:
a<- NEI[NEI$fips=="24510", ]
a_year<- a[a$year == "1999" | a$year == "2008",]

#Log transform the data to be closer to normal distribution:
a_year$Emissions<-a_year$Emissions+0.000000000000000000000000000000000001 #have to add small number because of nulls in data
a_year$logEmis<-log10(a_year$Emissions)

#Lets stack the results to object called vysl (as vysledky are results in my language):
vysl<-data.frame()

#Start with descriptive statistics - year and type, and sum, mean and median for log10(Emissions):
b<-aggregate(Emissions ~ year + type, a_year, sum)
vysl<-b
names(vysl)[3]<- "sum"
b<-aggregate(Emissions ~ year + type, a_year, mean)
vysl<-cbind(vysl, b[3])
names(vysl)[4]<- "mean"
b<-aggregate(Emissions ~ year + type, a_year, median)
vysl<-cbind(vysl, b[3])
names(vysl)[5]<- "median"
vysl

#Create collumn for storing results of t-tests:
vysl$t_test<-1

#Dirty hand work of testing and filling the vysl object. Note that singificance status is filled manually
#after visual insepction of t-test result:
## non-road
b<- t.test(x=subset(a_year, a_year$year == "1999" & a_year$type == "NON-ROAD")$logEmis, 
           y=subset(a_year, a_year$year == "2008" & a_year$type == "NON-ROAD")$logEmis)
vysl$t_test[1]<-b$p.value
b
vysl$t_test[2]<-"significant"

##nonpoint
b<- t.test(x=subset(a_year, a_year$year == "1999" & a_year$type == "NONPOINT")$logEmis, 
           y=subset(a_year, a_year$year == "2008" & a_year$type == "NONPOINT")$logEmis)
vysl$t_test[3]<-b$p.value
b
vysl$t_test[4]<-"non-significant"

##on-road
b<- t.test(x=subset(a_year, a_year$year == "1999" & a_year$type == "ON-ROAD")$logEmis, 
           y=subset(a_year, a_year$year == "2008" & a_year$type == "ON-ROAD")$logEmis)
vysl$t_test[5]<-b$p.value
b
vysl$t_test[6]<-"significant"

#point
b<- t.test(x=subset(a_year, a_year$year == "1999" & a_year$type == "POINT")$logEmis, 
           y=subset(a_year, a_year$year == "2008" & a_year$type == "POINT")$logEmis)
vysl$t_test[7]<-b$p.value
b
vysl$t_test[8]<-"significant"

#So, here we are
vysl
