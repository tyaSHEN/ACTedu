#Initialise####

#Set working directory
# replace location in line 9 with the working directory where you added the "updated STEP model delivery 022022" folder
# note to use / instead of \ when specifying your new working directory
# 
# setwd("Z:/My Documents/Demography/STEP model coding/updated STEP model delivery")

#Remove variables previously added to environment
rm(list=ls())
store = F
if(exists(".store")){
  store = .store
}

#Install all the required packages if necessary
if (!require("tidyverse")){
  install.packages("tidyverse", dep = T)
}

if (!require("data.table")){
  install.packages("data.table", dep = T)
}

if (!require("readxl")){
  install.packages("readxl", dep = T)
}

if (!require("doParallel")){
  install.packages("doParallel", dep = T)
}

#Set the number of iterations
iteration = 200
if(exists(".iteration")){
  iteration = .iteration
}

## Load files - code starts here####
#Loading main data files into R
load("output (2018)/0 data cleaning/cleaned data file.RData")

# R1 ####

## Determine R1 means ####
#Load SA2 level 2011-2031 age 4 population data
age4 <- fread("input/CMTEDD2023 age 4 input distributed.csv")

#Save number of preschool entries 
#Remove students who were previously enrolled in an ACT school, who do not have a home suburb listed, or whose home region is outside the ACT
#Return the number of rows, grouped by home suburb and year
R1 = transition[AcademicLevel == "00B" & is.na(ACL.pre) & !is.na(Home.Suburb) & HomeRegion!="Outside",][,.N,by=.(Home.Suburb,Year)]
R1 = full_join(age4 %>% filter(Year %in% c(2011:.ly)), R1, by = c("Year","SA2"="Home.Suburb"))

#Create a column for the R1 ratio (number of entries for a suburb and year divided by number of four-year-olds)
R1$r1 = R1$N/R1$Age.4

#Set r1 for suburbs where there are four year olds but the r1 value computed above is undefined equal to 0
#(Happens if no four-year-olds go to preschool)
R1[Age.4>0 & is.na(r1),r1 := 0]

#Set r1 for suburbs where the r1 calculated above is infinite equal to 1
#Happens if there were no four-year-olds and yet some preschoolers
R1[is.infinite(r1),r1 := 1]

#Load list of suburbs including year of opening for recent or new suburbs
recent.sa2 = fread("input/recent SA2_year_from_birth.csv")[,c(2,4)]

#Join suburb opening info with R1 data
R1 = left_join(R1,recent.sa2)

#For suburbs with no opening date specified, set open date to 2011
R1[is.na(year_open2),year_open2 := 2011]


## Determine R1 standard deviations ####
#Create a copy of the R1 data
R1.sd = copy(R1)

#For suburbs that opened in 2015 or earlier, create new columns with the standard deviations from 2016-2022, 2017-2022, and 2018-2022
# R1.sd[Year > (.ly-7) & year_open2<=(.ly-7), `:=`(`7` = sd(r1,na.rm = T)), by = .(SA2)]
# R1.sd[Year > (.ly-6) & year_open2<=(.ly-7), `:=`(`6` = sd(r1,na.rm = T)), by = .(SA2)]
# R1.sd[Year > (.ly-5) & year_open2<=(.ly-7), `:=`(`5` = sd(r1,na.rm = T)), by = .(SA2)]

#For suburbs that opened after 2015, create new columns with the standard deviations from 2016-2022, 2017-2022, 2018-2022, 2019-2022, 2020-2022
R1.sd[Year > (.ly-7) & year_open2<Year, `:=`(`7` = sd(r1,na.rm = T)), by = .(SA2)]
R1.sd[Year > (.ly-6) & year_open2<Year, `:=`(`6` = sd(r1,na.rm = T)), by = .(SA2)]
R1.sd[Year > (.ly-5) & year_open2<Year, `:=`(`5` = sd(r1,na.rm = T)), by = .(SA2)]
R1.sd[Year > (.ly-4) & year_open2<Year, `:=`(`4` = sd(r1,na.rm = T)), by = .(SA2)]
R1.sd[Year > (.ly-3) & year_open2<Year, `:=`(`3` = sd(r1,na.rm = T)), by = .(SA2)]

#Create a temporary object that gives the minimum R1 standard deviation for each suburb and gives you the year from which that standard deviation is calculated
tem2 = R1.sd %>% filter(Year == .ly) %>% pivot_longer(7:11,names_to = "duration",values_to = "R1.sd") %>% group_by(SA2) %>% 
  slice_min(R1.sd) %>% slice_min(duration) %>% mutate(y = .ly - as.numeric(duration)) %>% select(SA2,R1.sd,y)

tem = R1.sd %>% filter(Year == .ly) %>% pivot_longer(7:11,names_to = "duration",values_to = "R1.sd") %>% group_by(SA2) %>% 
  slice_max(R1.sd) %>% slice_min(duration) %>% mutate(y = .ly - as.numeric(duration)) %>% select(SA2,R1.sd,y)

tem$y=tem2$y
rm(tem2)

#Join the temporary object with the R1 table and remove temporary table
R1 = left_join(R1,tem)
rm(tem)

#Turn R1 into its copy
R1 = copy(R1)

#For years corresponding to the duration over which the sd is smallest, create a column with the R1 mean by suburb
#If the R1 mean is NA, make the R1 sd also NA
#Get rid of the y column
R1[Year > y, `:=`(R1.mean = mean(r1,na.rm = T)), by = .(SA2)][is.na(R1.mean),R1.sd := NA][,y:=NULL]

#Adjust for outliers and missing means
#Create a new table called R1.mean with just suburb, R1 mean, and R1 sd
R1.mean = unique(R1[Year == .ly], by = c("SA2","R1.mean")) %>% .[,.(SA2,R1.mean,R1.sd)]
#Compute the 50th, 80th and 90th percentile
q80 = quantile(R1.mean[R1.sd>0,R1.sd],0.8,na.rm = T)
q90 = quantile(R1.mean[R1.sd>0,R1.sd],0.9,na.rm = T)
q50 = quantile(R1.mean[R1.sd>0,R1.sd],0.5,na.rm = T)

# #For R1 standard deviations that are above the 90th percentile, change the mean to 1 and the standard deviation to the 80th percentile
# R1.mean[R1.sd>q90,`:=`(R1.mean=1,R1.sd=q80)]
# 
# #For R1 standard deviations that are above the 80th percentile (but below the 90th percentile), set the standard deviation equal to the 80th percentile
# R1.mean[R1.sd>q80,R1.sd := q80]

#If the standard deviation is 0 or missing, set the R1 mean equal to 1 and the standard deviation equal to the 50th percentile
R1.mean[R1.sd == 0|is.na(R1.sd),`:=`(R1.mean=1,R1.sd=q50)]
rm(q80, q90)

## Project forward ####
#Keeping the same R1 mean and sd values, project forward by a decade
R1.proj =  c()
for (i in (.ly+1):(.ly+10)) {
  tem = R1.mean
  tem$Year = i
  R1.proj= rbind(R1.proj,tem)
}
rm(tem, i)

# projection merge to 1.1 if r1 is over 1.15 in 6 years (optional)
n=6
limit = 1.15
centre = 1.1
#Have the mean hit 1.1 in 2028 (2022+n) onwards
R1.proj[R1.mean>limit & Year>(.ly+n),`:=` (R1.mean = centre,R1.sd = q50)]
#Have the mean gradually decrease between 2023 and 2028
R1.proj[R1.mean>limit, `:=` (R1.mean = seq(from = R1.mean[1], to = centre,length.out = n+1)[-1],
                             R1.sd = seq(from = R1.sd[1], to = q50,length.out = n+1)[-1]),by=SA2]

### ad hoc adjustment!
for (i in 1:9) {
  R1.proj[Year==(.ly+1+i),R1.sd := R1.sd*(1.08^i)]
}


## Create normal distributions ####
## Create a normal distribution of the preschool entries using the R1.mean as the mean and R1.sd as the sd for each suburb in each year
registerDoParallel(min(detectCores(),15))
R1.ALL = foreach(i=1:iteration,.packages = c("tidyverse","data.table"),.combine=rbind) %dopar% {  
  R1.proj[,r1 := rgamma(1,shape = (R1.mean^2)/(R1.sd^2),scale = (R1.sd^2)/R1.mean),by=.(SA2,Year)]
  R1.proj[r1<0,r1 := 0]
  R1.proj.iter = left_join(R1.proj,age4, by = c("SA2","Year")) %>% setDT()
  # population +/- 20% in 95% level
  R1.proj.iter[,age.4 := rnorm(1,Age.4,Age.4*0.2/2),by=.(SA2,Year)]
  # R1.proj.iter = R1.proj.iter %>% mutate(N = r1*Age.4,iter = i)
  R1.proj.iter = R1.proj.iter %>% mutate(N = r1*age.4,iter = i)
  R1.all = bind_rows(R1 %>% mutate(iter = i),R1.proj.iter) %>% arrange(SA2,Year,iter)
  R1.all
}
stopImplicitCluster()

#Create an R1 summary table with the medians, the 10th percentile and the 90th percentile of the preschool entries for each suburb and year
R1.sum = R1.ALL %>% group_by(SA2,Year) %>% summarise(m = median(N),q10 = quantile(N,0.1,na.rm = T),q90 = quantile(N,0.9,na.rm = T))

#store
if(store == T & !exists(".iteration")){
  save(list = c("R1.ALL","R1.sum"),file = "output (2018)/1 preschool/R1 by SA2 (iterations).RData")
}

#prepare file for examination in excel
R1.proj[,r1 := rnorm(1,R1.mean,R1.sd),by=.(SA2,Year)]
R1.proj[r1<0,r1 := 0]
R1.proj = left_join(R1.proj,age4, by = c("SA2","Year")) %>% mutate(N = R1.mean*Age.4)
R1.proj = bind_rows(R1,R1.proj) %>% arrange(SA2,Year)
R1.proj[Year>.ly,r1 := NA]

#store
if(store == T){
  fwrite(R1.proj,"output (2018)/1 preschool/R1 by SA2.csv")
}

# R2 ####
# Read in suburbs and a list of recently opened schools and the year they opened
recent.sa2 = fread("input/recent SA2_year_from_birth.csv")[,c(2,4)]
recent.sch = fread("input/recent list from Census (in-sample).csv") %>% .[AcademicLevel == "00B",]
recent.sch = recent.sch %>% mutate(year_open = Year) %>% select(SchoolName,year_open)

#Load transition data and clean
#Remove students who were previously enrolled in an ACT school, who do not have a home suburb listed, or whose home region is outside the ACT
#Return the number of rows, grouped by home suburb and year
R2.raw = transition[AcademicLevel == "00B" & is.na(ACL.pre) & !is.na(Home.Suburb) & HomeRegion!="Outside",][,.N,by=.(SchoolName, Home.Suburb,Year)]

#Create a new column called total with the sum of students from a given suburb in a give year and create a new column called r2 that
#divides the number of students from a given suburb in a given year that go to a given school by this total
R2.raw[,Total := sum(N),by=.(Home.Suburb,Year)][,r2 := N/Total]

#Join this R2 data with school open years
R2 = left_join(R2.raw,recent.sch, by= c("SchoolName"))

#If school open year not give, set to 2010
R2[is.na(year_open),year_open:= 2010]

#Join with suburb opening and delay one year
R2 = left_join(R2,recent.sa2, by= c("Home.Suburb"="SA2"))
R2[!is.na(year_open2),year_open2:= year_open2+1][is.na(year_open2),year_open2:= 2010]

#Temporarily reshape the data to be wide rather than long
R2.wide = dcast(R2, SchoolName+Home.Suburb+year_open+year_open2 ~ Year, value.var = "r2")
R2.wide[,as.character(2011:.ly):=lapply(.SD, function(x) ifelse(is.na(x),0,x)),.SDcols = as.character(2011:.ly)]

#Reshape the data to be long
R2 = melt(R2.wide,id = 1:4,variable.name = "Year",value.name = "r2")
R2$Year = as.numeric(as.character(R2$Year))

#Remove r2 values for if the year in question is earlier than the year the school (year_open) or the suburb opened (year_open2)
R2[Year<year_open,r2:=NA][,year_open:=NULL]  
R2[Year<year_open2,r2:=NA][,year_open2:=NULL]

# # Covenant Christian School	missed suburb information in 2022. Turn this R2 to NA
if(.ly>=2022){
  R2[Year == 2022 & SchoolName=="Covenant Christian School", r2:=NA]
}
if(.ly>=2019){
  R2[Year == 2019 & SchoolName=="Canberra Girls Grammar School", r2:=NA]
}
if(.ly>=2015){
  R2[Year == 2015 & SchoolName=="Brindabella Christian College", r2:=NA]
}

## Determine R2 standard deviations####
# Extract R2 with at least 5 year of historical data. R2 less than five years will be forced to use five-year average
R2.sd = copy(R2)

#Get all the school+suburbs that have r2 values for each of the last 5 years 
R2.sd5 = R2.sd[Year > (.ly-5) & !is.na(r2) & r2 > 0, N := .N, by = .(Home.Suburb,SchoolName)][N==5&Year == .ly,.(SchoolName,Home.Suburb,N)]

#Join with R2sd values
R2.sd = left_join(R2.sd,R2.sd5)
R2.sd = copy(R2.sd)

#Calculate the 3-7 year standard deviations for school+suburbs that have at least 5 recent years of r2 values
R2.sd[Year > (.ly-7)& N==5, `:=`(`7` = sd(r2,na.rm = T)), by = .(Home.Suburb,SchoolName)]
R2.sd[Year > (.ly-6)& N==5, `:=`(`6` = sd(r2,na.rm = T)), by = .(Home.Suburb,SchoolName)]
R2.sd[Year > (.ly-5)& N==5, `:=`(`5` = sd(r2,na.rm = T)), by = .(Home.Suburb,SchoolName)]
R2.sd[Year > (.ly-4)& N==5, `:=`(`4` = sd(r2,na.rm = T)), by = .(Home.Suburb,SchoolName)]
R2.sd[Year > (.ly-3)& N==5, `:=`(`3` = sd(r2,na.rm = T)), by = .(Home.Suburb,SchoolName)]

#Reshape the data to be long, select only periods that minimise the sd
#Create a temporary object that gives the minimum R1 standard deviation for each suburb and gives you the year from which that standard deviation is calculated
tem = R2.sd %>% filter(Year == .ly) %>% pivot_longer(6:10,names_to = "duration",values_to = "R2.sd") %>% group_by(Home.Suburb,SchoolName) %>% 
  mutate(R2.sd = ifelse(is.na(R2.sd)&duration==5,0,R2.sd)) %>% slice_min(R2.sd) %>% slice_min(duration) %>% 
  mutate(y = .ly - as.numeric(duration),y2 = .ly - as.numeric(duration)) %>% select(Home.Suburb,SchoolName,R2.sd,y,y2)

tem2 = R2.sd %>% filter(Year == .ly) %>% pivot_longer(6:10,names_to = "duration",values_to = "R2.sd") %>% group_by(Home.Suburb,SchoolName) %>% 
  mutate(R2.sd = ifelse(is.na(R2.sd)&duration==5,0,R2.sd)) %>% slice_max(R2.sd) %>% slice_min(duration) %>% 
  mutate(y = .ly - as.numeric(duration),y2 = .ly - as.numeric(duration)) %>% select(Home.Suburb,SchoolName,R2.sd,y,y2)

tem$y2=tem2$y2
rm(tem2)

R2 = left_join(R2,tem)
R2 = copy(R2)

## Determine R2 means and growth####

#Create mean R2 column 
R2[Year > y, `:=`(R2.mean = mean(r2,na.rm = T)), by = .(Home.Suburb,SchoolName)]
R2[Year > y2, `:=`(R2.sd = sd(r2,na.rm = T)), by = .(Home.Suburb,SchoolName)][is.na(R2.mean),R2.sd := NA]
R2[,y2:=NULL]

#Get the number of years since the start of the minimum SD calculation that the R2 is 0
tem = R2[Year > y & r2 == 0, .N, by = .(Home.Suburb,SchoolName)]
R2 = left_join(R2,tem)
R2 = copy(R2)

#Get annual growth in R2 for school-suburbs that have no missing R2 values
R2[Year > y&is.na(N),R2.growth:=lead(r2)-r2,by = .(Home.Suburb,SchoolName)]

#For school-suburbs that have years of 0, create a new column x and set it to the number of years of 0
R2[!is.na(R2.growth), x:=.N, by = .(Home.Suburb,SchoolName)]

#If the year is 2021 and the number of 0s is 1, set the growth to NA to get rid of new school
R2[Year == .ly-1 & x == 1, R2.growth:=NA, by = .(Home.Suburb,SchoolName)]

#Change the R2 growth column to be the mean R2 growth
R2 = R2 %>% group_by(Home.Suburb,SchoolName) %>% mutate(R2.growth=mean(R2.growth,na.rm=T))
setDT(R2)

#If the R2 growth is NA, set it to zero, if the R2 mean is NA, set the R2 growth to 0, get rid of x, y, and N columns
R2[is.na(R2.growth),R2.growth:=0][is.na(R2.mean),R2.growth:=NA][,x:=NULL][,y:=NULL][,N:=NULL]

#Create a new column with the sum of the R2 mean and the R2 growth
R2[,R2.mean.growth := R2.mean]

#Join with the r2s and number of students per suburb to each school
R2 = left_join(R2,R2.raw)
R2[r2==0,N:=0]

# Examine for very high total R2
examine = R2 %>% filter(Year == .ly) %>% group_by(Home.Suburb) %>% summarise(tot=sum(R2.mean.growth)) %>% arrange(-tot)
View(examine)

## Manually adjust select R2s####
# Three schools need manual adjustment:
#  two recent school (Evelyn Scott School, Throsby Primary School)
#  one changed PEA (Franklin Early Childhood School)
very.recent.sch=recent.sch[year_open>(.ly-2),SchoolName]
# very.recent.sch=append(very.recent.sch,"Franklin Early Childhood School")

#Get the R2 info for the three schools for 2022
SA2.recent.sch = R2[SchoolName %in% very.recent.sch & Year == .ly,Home.Suburb]

#Get all the adjusted R2 means for 2022
R2.mean = R2[Year == .ly,.(Home.Suburb,SchoolName,R2.mean.growth,R2.sd)]

# examine this data to spot the change in time series in r2
examine = R2[(Home.Suburb %in% SA2.recent.sch | SchoolName %in% very.recent.sch) &
               Year > (.ly-5) ][,`:=`(Avg = mean(N,na.rm = T),Avg.sd = sd(r2,na.rm = T)),by = .(Home.Suburb,SchoolName)] %>%
  filter(Avg>5) %>% arrange(-Avg.sd,Home.Suburb,SchoolName,Year)
View(examine)
# look for very extreme R2.growth, if there is sharp turn in recent year change AS 1.1

## ASSUMPTION 1.1 (review each year)####
#Adjust R2s to match assumptions
if(.ly==2022){
  R2.adj <- read_excel("assumption/Preschool/AS 1.1 R2 adjustment.xlsx") %>% as.data.table()
  R2.mean = left_join(R2.mean,R2.adj)
  
  R2.mean.adj = c()
  for (i in unique(R2.mean$Home.Suburb)) {
    adj = sum(R2.mean[Home.Suburb == i]$R2.mean.adj,na.rm = T)
    adj = 1-ifelse(adj>0,adj,0)
    tem = R2.mean %>% filter(Home.Suburb == i,is.na(R2.mean.adj)) %>% mutate(R2.mean.adj = adj/sum(R2.mean.growth)*R2.mean.growth)
    R2.mean.adj = rbind(R2.mean.adj,tem)
    tem = R2.mean %>% filter(Home.Suburb == i,!is.na(R2.mean.adj))
    R2.mean.adj = rbind(R2.mean.adj,tem)
  }
  
  R2.mean = R2.mean.adj
  R2.mean = R2.mean %>% filter(!is.na(R2.mean.adj))
}else{
  R2.mean[,R2.mean.adj := 1/sum(R2.mean.growth)*R2.mean.growth, by = Home.Suburb]
}

## Project adjustments forward
R2.proj =  c()
for (i in (.ly+1):(.ly+10)) {
  tem = R2.mean
  tem$Year = i
  R2.proj= rbind(R2.proj,tem)
}

## ASSUMPTION 1.2 ####
# add new suburbs or new school
R2.add.sub = fread("assumption/Preschool/AS 1.2 New Suburbs or New School (in-sample) 2018.csv") %>% as.data.table()

#Create a list of all the new suburbs and their R2s to each school for the given years
R2.add.sub = R2.add.sub %>% filter(!is.na(SchoolName)) 
R2.new.sub =c()
for (i in 1:nrow(R2.add.sub)) {
  tem = data.frame(Home.Suburb = R2.add.sub[i,Home.Suburb],
                   SchoolName = R2.add.sub[i,SchoolName],
                   Year = c(R2.add.sub[i,year.from]:R2.add.sub[i,year.to]),
                   R2.adj = R2.add.sub[i,R2.adj])
  R2.new.sub = bind_rows(R2.new.sub,tem)
}
setDT(R2.new.sub)

# add new suburbs or new school
R2.add.sub2 = read_excel("assumption/Preschool/AS 1.2 New Suburbs or New School (Feb 23).xlsx") %>% as.data.table()
# if there is existing R2 for suburb but school name is NA, change the adjusted R2 mean in R2.proj to 0 for all schools
if("TRUE" %in% is.na(R2.add.sub2$SchoolName)){
  R2.add.sub2.rm = R2.add.sub2 %>% filter(is.na(SchoolName))
  for (i in 1:nrow(R2.add.sub2.rm)) {
    R2.proj[Year>=R2.add.sub2.rm[i,year.from] & Home.Suburb == R2.add.sub2.rm[i,Home.Suburb], R2.mean.adj := 0]
    R2.new.sub[Year>=R2.add.sub2.rm[i,year.from] & Home.Suburb == R2.add.sub2.rm[i,Home.Suburb], R2.adj := 0]
  }
}

#Create a list of all the new suburbs and their R2s to each school for the given years
R2.add.sub2 = R2.add.sub2 %>% filter(!is.na(SchoolName)) 
R2.new.sub2 =c()
for (i in 1:nrow(R2.add.sub2)) {
  tem = data.frame(Home.Suburb = R2.add.sub2[i,Home.Suburb],
                   SchoolName = R2.add.sub2[i,SchoolName],
                   Year = c(R2.add.sub2[i,year.from]:R2.add.sub2[i,year.to]),
                   R2.adj = R2.add.sub2[i,R2.adj])
  R2.new.sub2 = bind_rows(R2.new.sub2,tem)
}

R2.new.sub = full_join(R2.new.sub,R2.new.sub2,by=c("Home.Suburb","SchoolName","Year"))
R2.new.sub = R2.new.sub %>% mutate(R2.adj = ifelse(is.na(R2.adj.y),R2.adj.x,R2.adj.y)) %>% select(-R2.adj.x,-R2.adj.y)
R2.new.sub = filter(R2.new.sub,Year >.ly)

#Join the projections with the new suburb info and replace the adjusted mean R2 for each of the new suburb-schools
R2.proj = full_join(R2.proj,R2.new.sub)
R2.proj[!is.na(R2.adj),R2.mean.adj := R2.adj]
R2.proj[,R2.adj:=NULL]
R2.proj = R2.proj[R2.mean.adj>0 & Year >.ly & Year <= (.ly+10)]

# rescale to 1
R2.proj[,R2.mean.adj := 1/sum(R2.mean.adj)*R2.mean.adj, by = .(Home.Suburb,Year)]

#Add new columns to R2.proj so you can bind with R2; get rid of any rows where the average of the R2 mean is not greater than 0
R2.proj[,`:=`(R2.mean=R2.mean.adj,r2 = NA,R2.sd = R2.sd,N = NA,Total = NA)][,R2.mean.adj:=NULL][,R2.mean.growth:=NULL]
R2 = copy(R2)
# R2[,AVG := mean(R2.mean,na.rm = T),by=.(Home.Suburb,SchoolName)]
# R2 = R2[AVG>0][,AVG:=NULL]

R2.proj = bind_rows(R2,R2.proj)
R2.proj = copy(R2.proj)
R2.proj[,entries := N]
R2.proj[,N:=NULL]
tem = R2.proj %>% filter(Year<=.ly,R2.mean>0) %>% summarise(x=mean(R2.sd,na.rm = T)) %>% pull(x)
R2.proj[Year > .ly & is.na(R2.sd), R2.sd:=tem]

setorder(R2.proj,Home.Suburb,SchoolName)

### ad hoc adjustment!
for (i in 1:9) {
  R2.proj[Year==(.ly+1+i),R2.sd := R2.sd*(1.08^i)]
}


#store the R2 values
if(store == T){
  tem = left_join(R1.proj %>% select(-year_open2),R2.proj %>% select(-Total),by=c("SA2" = "Home.Suburb","Year")) %>% 
  mutate(entries = ifelse(Year>.ly,N*R2.mean,entries)) %>% arrange(SA2,SchoolName)
  fwrite(tem, "output (2018)/1 preschool/R2 by SA2 & School.csv")
}

# Entries in ACT ####
## Projected entries in ACT (R1 & R2) ####
#Get ride of the Total and year_open2 columns in R2.proj and R1.ALL, respectively
R2.proj[,Total:=NULL]
R1.ALL[,year_open2:=NULL]

#full join R1.ALL and R2.proj
Entries.in.ACT = R1.ALL[R2.proj,on =.(SA2 = Home.Suburb,Year=Year),allow.cartesian=TRUE]

#Round the counts
Entries.in.ACT[,N := round(N)]

#Sample from a multinomial distribution of size N with probability equal to the R2.mean
Entries.in.ACT = copy(Entries.in.ACT)
estBetaParams <- function(mu, sd) {
  if(mu>0 & mu <1 & sd^2<(mu*(1-mu)) & sd>0){
    alpha <- ((1 - mu) / (sd^2) - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)
    tem = rbeta(1,alpha,beta)
    tem
  }else{
    mu
  }
}
Entries.in.ACT[Year>.ly,r2 := estBetaParams(R2.mean,R2.sd),by=.(SA2,Year,iter,SchoolName)]
Entries.in.ACT[Year>.ly & is.na(r2),r2 := R2.mean]
Entries.in.ACT[,r2 := 1/sum(r2)*r2, by = .(SA2,Year,iter)]
Entries.in.ACT[Year>.ly,entries := rmultinom(1,N,r2),by=.(SA2,Year,iter)]

#store
if(store == T & !exists(".iteration")){
  save(Entries.in.ACT, file = "output (2018)/1 preschool/R2 by SA2 & School (iterations).RData")
}

#
Entries.in.ACT.school = Entries.in.ACT[,.(entries = sum(entries,na.rm = T)),by=.(SchoolName,Year,iter)]
Entries.in.ACT.school.sum = Entries.in.ACT.school %>% group_by(SchoolName,Year) %>% summarise(median = median(entries,na.rm = T),lower=quantile(entries,0.1,na.rm=T),upper=quantile(entries,0.9,na.rm=T))
View(Entries.in.ACT.school.sum)

#store
if(store == T){
  fwrite(Entries.in.ACT.school.sum, "output (2018)/1 preschool/School entries inside ACT.csv")
}

# Entries outside ACT ####
#number of entries outside of ACT
#Read in a list of the recent schools that have a preschool level
recent.sch = fread("input/recent list from Census (in-sample).csv") %>% .[AcademicLevel == "00B",]

#Change the column 'Year' to 'year_open' and just keep the SchoolName and year_open columns
recent.sch = recent.sch %>% mutate(year_open = Year) %>% select(SchoolName,year_open)

#Create a table with just the students who are in preschool for the first time whose home region is outside the ACT
Entries.outside.ACT = transition[AcademicLevel == "00B" & is.na(ACL.pre) &  HomeRegion=="Outside",][,.N,by=.(SchoolName,Year)]

#Join the outside preschoolers with the recent ACT schools
Entries.outside.ACT = left_join(Entries.outside.ACT,recent.sch, by= c("SchoolName"))

#If the school year of opening isn't specified, treat it as 2010
Entries.outside.ACT[is.na(year_open),year_open:= 2008]

#Reshape Entries.outside.ACT to be wide
Entries.outside.ACT.wide = dcast(Entries.outside.ACT, SchoolName+year_open ~ Year, value.var = "N")

#If a cell in the columns from 2011 to .ly are NA, set them to 0
Entries.outside.ACT.wide[,as.character(2009:.ly):=lapply(.SD, function(x) ifelse(is.na(x),0,x)),.SDcols = as.character(2009:.ly)]

#Melt (make it wide to long) the Entries.outside.ACT table
Entries.outside.ACT = melt(Entries.outside.ACT.wide,id = 1:2,variable.name = "Year",value.name = "entries.out")
Entries.outside.ACT$Year = as.numeric(as.character(Entries.outside.ACT$Year))

#If the year is earlier than a school's opening, set the outside entries to NA
Entries.outside.ACT[Year<year_open,entries.out:=NA][,year_open:=NULL] 

# Covenant Christian School	missed suburb information in 2022. Turn this R2 to NA
if(.ly>=2022){
  Entries.outside.ACT[Year == 2022 & SchoolName=="Covenant Christian School", entries.out:=NA]
}

#Get the mean and sd of the entries from outside the ACT in the last 5 years
Entries.outside.ACT = Entries.outside.ACT[Year > (.ly-5), `:=`(entries.out.mean = mean(entries.out,na.rm = T),entries.out.sd = sd(entries.out,na.rm = T)), by = .(SchoolName)]

### ad hoc adjustment!
# Entries.outside.ACT[,entries.out.sd*1.5]

## Projected entries outside ACT ####
#Project outside ACT school entires forward for the next decade
for (i in (.ly+1):(.ly+10)) {
  tem = Entries.outside.ACT[Year == .ly]
  tem[is.na(entries.out.mean),entries.out.mean := 0]
  tem[is.na(entries.out.sd),entries.out.sd := median(tem$entries.out.sd,na.rm = T)]
  tem$Year = i
  Entries.outside.ACT= rbind(Entries.outside.ACT,tem)
}
Entries.outside.ACT[Year > .ly, entries.out := entries.out.mean]

#Join the projected school entries (Entries.outside.ACT) with the other entry information (Entries.in.ACT.school)
Entries.out = left_join(Entries.in.ACT.school,Entries.outside.ACT)

#Use the means and standard deviations of the entries out to sample from a negative binomial distribution
Entries.out = copy(Entries.out)

### ad hoc adjustment!
for (i in 1:9) {
  Entries.out[Year==(.ly+1+i),entries.out.sd := entries.out.sd*(1.08^i)]
}

Entries.out[Year > .ly,entries.out := rnbinom(1,size = ifelse(entries.out.mean^2/(entries.out.sd^2-entries.out.mean)>0,entries.out.mean^2/(entries.out.sd^2-entries.out.mean),Inf),mu=entries.out.mean),by = .(SchoolName,Year, iter)]
Entries.out[is.na(entries.out) & entries >0,entries.out := 0]

#store
if(store == T){
  fwrite(Entries.out %>% filter(iter==1) %>% select(-iter,-entries),"output (2018)/1 preschool/School entries outside ACT.csv")
}

# Projected preschool entries ####
#Add the entries from within the ACT to the entries from outside
Entries = copy(Entries.out)
Entries[,entries.out.mean:= NULL][,entries.out.sd:= NULL][,Entries := entries+entries.out]
Entries = Entries %>% filter(!(SchoolName=="Urambi Primary School"))

#Create an summary table with the medians, the 10th percentile and the 90th percentile of the preschool entries for each school and year
Entries.sum = Entries %>% group_by(SchoolName,Year) %>% summarise(median = median(Entries,na.rm = T),q2.5= quantile(Entries,0.025,na.rm=T),q10=quantile(Entries,0.1,na.rm=T),q90=quantile(Entries,0.9,na.rm=T),q97.5= quantile(Entries,0.975,na.rm=T))
View(Entries.sum)

#store entries
if(store == T){
  fwrite(Entries.sum,"output (2018)/1 preschool/School entries.csv")
  save(Entries, file=paste0("output (2018)/1 preschool/School entries.RData"))
  #save.image(file=paste0("output (2018)/1 preschool/1 preschool.RData"))
}
