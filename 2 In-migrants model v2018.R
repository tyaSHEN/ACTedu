rm(list=ls())
store = F
if(exists(".store")){
  store = .store
}

if (!require("tidyverse")){
  install.packages("tidyverse", dep = T)
  library(tidyverse)
}
if (!require("data.table")){
  install.packages("data.table", dep = T)
}
if (!require("smooth")){
  install.packages("smooth", dep = T)
  library(tidyverse)
}
if (!require("readxl")){
  install.packages("readxl", dep = T)
}
if (!require("doParallel")){
  install.packages("doParallel", dep = T)
}

iteration = 10
if(exists(".iteration")){
  iteration = .iteration
}

# code starts from here ##
#Loading main data files into R
load("output (2018)/0 data cleaning/cleaned data file.RData")

# In-migrant in ACT ####
## existing suburbs ####
MIG = transition[AcademicLevel != "00B" & is.na(ACL.pre) & !is.na(Home.Suburb) & HomeRegion!="Outside"&Year>2008,][,.N,by=.(Home.Suburb,Year)]

Mig = MIG%>% arrange(Home.Suburb) %>%  pivot_wider(id_cols = Year,values_from = N,names_from = Home.Suburb)
Mig = Mig %>% pivot_longer(c(2:ncol(Mig)),names_to = "Home.Suburb",values_to = "N") %>% as.data.table()
Mig[is.na(N),N := 0]

recent.sa2 = fread("input/recent SA2_year_from_birth.csv")[,c(2,4)]
Mig = left_join(Mig,recent.sa2,by=c("Home.Suburb"="SA2"))
Mig[!is.na(year_open2) & Year<(year_open2+1),N := NA][is.na(year_open2),year_open2 := 2008]
Mig = Mig[year_open2<(.ly-1)]
#Create a copy of the R1 data
Mig.sd = copy(Mig)

#For suburbs that opened in 2015 or earlier, create new columns with the standard deviations from 2016-2022, 2017-2022, and 2018-2022
Mig.sd[Year > (.ly-7) & year_open2<=(.ly-7), `:=`(`7` = sd(N,na.rm = T)), by = .(Home.Suburb)]
Mig.sd[Year > (.ly-6) & year_open2<=(.ly-7), `:=`(`6` = sd(N,na.rm = T)), by = .(Home.Suburb)]
Mig.sd[Year > (.ly-5) & year_open2<=(.ly-7), `:=`(`5` = sd(N,na.rm = T)), by = .(Home.Suburb)]

#For suburbs that opened after 2015, create new columns with the standard deviations from 2016-2022, 2017-2022, 2018-2022, 2019-2022, 2020-2022
Mig.sd[Year > (.ly-7) & year_open2>(.ly-7) & year_open2<Year, `:=`(`7` = sd(N,na.rm = T)), by = .(Home.Suburb)]
Mig.sd[Year > (.ly-6) & year_open2>(.ly-7) & year_open2<Year, `:=`(`6` = sd(N,na.rm = T)), by = .(Home.Suburb)]
Mig.sd[Year > (.ly-5) & year_open2>(.ly-7) & year_open2<Year, `:=`(`5` = sd(N,na.rm = T)), by = .(Home.Suburb)]
Mig.sd[Year > (.ly-4) & year_open2>(.ly-7) & year_open2<Year, `:=`(`4` = sd(N,na.rm = T)), by = .(Home.Suburb)]
Mig.sd[Year > (.ly-3) & year_open2>(.ly-7) & year_open2<Year, `:=`(`3` = sd(N,na.rm = T)), by = .(Home.Suburb)]

#Create a temporary object that gives the minimum R1 standard deviation for each suburb and gives you the year from which that standard deviation is calculated
tem = Mig.sd %>% filter(Year == .ly) %>% pivot_longer(5:ncol(Mig.sd),names_to = "duration",values_to = "Mig.sd") %>% group_by(Home.Suburb) %>% 
  slice_min(Mig.sd) %>% slice_min(duration) %>% mutate(y = .ly - as.numeric(duration)) %>% select(Home.Suburb,Mig.sd,y)

#Join the temporary object with the R1 table and remove temporary table
Mig = left_join(Mig,tem)
rm(tem)

Mig[Year <= y,Mig.sd := NA]
# Mig[y>(.ly-5),y := (.ly-5)]
Mig[,y := 5][,year_open2:=NULL]
Mig.suburb.proj = Mig
for (s in unique(Mig$Home.Suburb)) {
  tem = Mig[Home.Suburb == s & Year > (.ly-y)] %>% arrange(Year)
  x = tem$Mig.sd[length(tem$Mig.sd)]
  y = tem$y[1]
  try({tem = sma(tem$N,order = y,h=5)}, silent = TRUE)
  try({tem = sma_old(tem$N,order = y,h=5)}, silent = TRUE)
  tem = c(tem[["forecast"]],rep(tem[["forecast"]][5],5))
  tem = data.frame(Home.Suburb = s, Year = c((.ly+1):(.ly+10)),N = tem,Mig.sd = x)
  Mig.suburb.proj = bind_rows(Mig.suburb.proj,tem)
}

Mig.suburb.proj[,y:=NULL]

## redistribute in-migrant to (new suburbs) ####
# use in-migrant/school age population (age5-age17) ratios from a similar recent suburb
#ASSUMPTION 2.1 (review each year)
new.sub = read_excel("assumption/In-migrant/AS 2.1 New Suburbs (in-sample) 2018.xlsx")
new_sub = crossing(Home.Suburb = setdiff(new.sub$`new SA2`,Mig.suburb.proj$Home.Suburb),Year = c((.ly+1):(.ly+10)))

Mig.suburb.proj = bind_rows(Mig.suburb.proj,new_sub)

pop <- read_csv("input/CMTEDD2023 age 5-17 input distributed (v2).csv") %>% filter(Year %in% c(2011:(.ly+10)))
pop <- pivot_longer(pop, c(3:15)) %>% group_by(SA2,Year) %>% summarise(pop = sum(value))

Mig.suburb.proj = left_join(Mig.suburb.proj,pop,c("Home.Suburb"="SA2","Year"))
Mig.suburb.proj[Home.Suburb%in% new.sub$`new SA2` & Year >.ly,N := NA]

ratio = Mig.suburb.proj %>% 
  filter(Home.Suburb %in% unique(new.sub$`ratio from`),!is.na(N)) %>% group_by(Home.Suburb) %>% 
  mutate(ratio = N/pop,Year = Year-min(Year))

new_sub = Mig.suburb.proj %>% filter(Home.Suburb%in% new.sub$`new SA2` & Year >.ly)
new_sub = left_join(new_sub,new.sub,by=c("Home.Suburb"="new SA2"))
new_sub[Year<year_open2, pop := NA]
new_sub = copy(new_sub)
new_sub[Year>=year_open2, year := Year-year_open2]
new_sub = left_join(new_sub,ratio[,c(1,2,6)],by = c("year"="Year","ratio from"="Home.Suburb"))
new_sub = new_sub %>% mutate(N.adj = pop*ratio) %>% select(Home.Suburb,Year,N.adj)

# join the new suburb and resale the in-migration number to the same total
new_sub.tot = new_sub %>% group_by(Year) %>% summarise(tot.new= sum(N.adj,na.rm = T))
Mig.suburb.proj.tot = Mig.suburb.proj %>% group_by(Year) %>% summarise(tot= sum(N,na.rm = T))
new_sub.tot = left_join(new_sub.tot,Mig.suburb.proj.tot) %>% 
  mutate(tot.new = tot-tot.new,ratio = tot.new/tot) %>% select(Year,ratio)
Mig.suburb.proj = left_join(Mig.suburb.proj,new_sub.tot)
Mig.suburb.proj[is.na(ratio),ratio:=1]

Mig.suburb.proj = left_join(Mig.suburb.proj,new_sub)
Mig.suburb.proj[!is.na(N),N.adj := N*ratio]
Mig.suburb.proj[,ratio := NULL]
Mig.suburb.proj[Year>.ly&is.na(N),Mig.sd:=mean(Mig.suburb.proj$Mig.sd,na.rm = T)]

### ad hoc adjustment!
for (i in 1:9) {
  Mig.suburb.proj[Year==(.ly+1+i),Mig.sd := Mig.sd*(1.08^i)]
}

#store
if(store == T){
  fwrite(Mig.suburb.proj, "output (2018)/2 in-migrant/In-migration by SA2.csv")
}

registerDoParallel(min(detectCores(),15))
Mig.suburb.All = foreach(i=1:iteration,.packages = c("tidyverse","data.table"),.combine=rbind) %dopar% {  
  Mig.suburb.proj[Year > .ly,in.mig := rnbinom(1,size = ifelse(N.adj^2/(Mig.sd^2-N.adj)>0,N.adj^2/(Mig.sd^2-N.adj),Inf),mu=N.adj),by = .(Home.Suburb,Year)]
  Mig.suburb.proj = Mig.suburb.proj %>% mutate(iter = i)
  Mig.suburb.proj
}
stopImplicitCluster()

#Create an R1 summary table with the medians, the 10th percentile and the 90th percentile of the preschool entries for each suburb and year
Mig.suburb.sum = Mig.suburb.All %>% mutate(in.mig = ifelse(is.na(in.mig),N.adj,in.mig)) %>% group_by(Home.Suburb,Year) %>% summarise(m = median(in.mig,na.rm = T),q10 = quantile(in.mig,0.1,na.rm = T),q90 = quantile(in.mig,0.9,na.rm = T))

#store
if(store == T & !exists(".iteration")){
  save(list = c("Mig.suburb.All","Mig.suburb.sum"),file = "output (2018)/2 in-migrant/In-migration by SA2 (iterations).RData")
}

## R3 ####
MIG.SUB.SCH = transition[AcademicLevel != "00B" & is.na(ACL.pre) & !is.na(Home.Suburb) & HomeRegion!="Outside"&Year>2008,][,.N,by=.(Home.Suburb,SchoolName,AcademicLevel,Year)]

MIG.SUB.SCH.wide = pivot_wider(MIG.SUB.SCH,names_from = Year,values_from = N) %>% as.data.table()
MIG.SUB.SCH.wide[,as.character(2009:.ly):=lapply(.SD, function(x) ifelse(is.na(x),0,x)),.SDcols = as.character(2009:.ly)]

MIG.SUB.SCH = melt(MIG.SUB.SCH.wide,id = 1:3,variable.name = "Year",value.name = "mig")

# remove 0 from new school or new suburb
recent.sa2 = fread("input/recent SA2_year_from_birth.csv")[,c(2,4)]
recent.sch = fread("input/recent list from Census (in-sample).csv")
recent.sch = recent.sch %>% mutate(year_open = Year) %>% select(SchoolName,AcademicLevel,year_open)

MIG.SUB.SCH = left_join(MIG.SUB.SCH,recent.sch, by= c("SchoolName","AcademicLevel"))
MIG.SUB.SCH[is.na(year_open),year_open:= 2008]
MIG.SUB.SCH = left_join(MIG.SUB.SCH,recent.sa2, by= c("Home.Suburb"="SA2"))
MIG.SUB.SCH[!is.na(year_open2),year_open2:= year_open2][is.na(year_open2),year_open2:= 2008]
MIG.SUB.SCH$Year = as.numeric(as.character(MIG.SUB.SCH$Year))

MIG.SUB.SCH[Year<year_open,mig:=NA][,year_open:=NULL]  
MIG.SUB.SCH[Year<year_open2,mig:=NA][,year_open2:=NULL]

MIG.SUB.SCH = left_join(Mig.suburb.proj[,c(1,2,6)],MIG.SUB.SCH)
MIG.SUB.SCH$r3 = MIG.SUB.SCH$mig/MIG.SUB.SCH$N.adj

MIG.SUB.SCH[Year > (.ly-5),`:=`(r3.mean=mean(r3,na.rm = T),r3.sd = sd(r3,na.rm = T)), by=.(Home.Suburb,SchoolName,AcademicLevel)]

# keep the one with r3 over 0
subset = MIG.SUB.SCH[Year == .ly & r3.mean>0,.(Home.Suburb,SchoolName,AcademicLevel)]
R3 = left_join(subset,MIG.SUB.SCH) %>% select(-N.adj)

R3.mean = MIG.SUB.SCH[Year == .ly & r3.mean>0,.(Home.Suburb,SchoolName,AcademicLevel,r3.mean,r3.sd)]

R3.proj =  c()
for (i in (.ly+1):(.ly+10)) {
  tem = R3.mean
  tem$Year = i
  R3.proj= rbind(R3.proj,tem)
}

# ASSUMPTION 2.2 add new suburb and new school
R3.add <- read_excel("assumption/In-migrant/AS 2.2 R3 New Suburbs and new school (Feb 23).xlsx") %>% as.data.table()

R3.new.sub.1 =c()
for (i in 1:nrow(R3.add)) {
  tem = data.frame(Home.Suburb = R3.add[i,Home.Suburb],
                   SchoolName = R3.add[i,SchoolName],
                   AcademicLevel = R3.add[i,AcademicLevel],
                   Year = c(R3.add[i,year.from]:R3.add[i,year.to]),
                   R3.adj = R3.add[i,r3.adj])
  R3.new.sub.1 = bind_rows(R3.new.sub.1,tem)
}

R3.add <- read_excel("assumption/In-migrant/AS 2.2 R3 New Suburbs and new school (in-sample) 2018.xlsx") %>% as.data.table()

R3.new.sub.2 =c()
for (i in 1:nrow(R3.add)) {
  tem = data.frame(Home.Suburb = R3.add[i,Home.Suburb],
                   SchoolName = R3.add[i,SchoolName],
                   AcademicLevel = R3.add[i,AcademicLevel],
                   Year = c(R3.add[i,year.from]:R3.add[i,year.to]),
                   R3.adj.2 = R3.add[i,r3.adj])
  R3.new.sub.2 = bind_rows(R3.new.sub.2,tem)
}

R3.new.sub = full_join(R3.new.sub.1,R3.new.sub.2)
R3.new.sub$R3.adj = ifelse(is.na(R3.new.sub$R3.adj),R3.new.sub$R3.adj.2,R3.new.sub$R3.adj)
R3.new.sub = select(R3.new.sub,-R3.adj.2)
  
R3.proj = full_join(R3.proj,R3.new.sub)
R3.proj[!is.na(R3.adj),r3.mean := R3.adj][,R3.adj := NULL]
R3.proj = copy(R3.proj)
# rescale to 1
R3.proj[,r3.rescale := 1/sum(r3.mean)*r3.mean, by = .(Home.Suburb,Year)]
R3.proj[is.na(r3.sd),r3.sd := mean(R3.mean$r3.sd,na.rm = T)]

### ad hoc adjustment!
for (i in 1:9) {
  R3.proj[Year==(.ly+1+i),r3.sd := r3.sd*(1.08^i)]
}

R3 = bind_rows(R3,R3.proj)
R3[, r3.mean := NULL]

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


R3.All = left_join(Mig.suburb.All[,c(1,2,6,7,8)],R3)
R3.All[Year > .ly,in.mig := ifelse(is.na(in.mig),0,in.mig)]
R3.All[Year > .ly & is.na(r3.rescale),r3.rescale := 0]
R3.All[Year > .ly & is.na(r3.sd),r3.sd := 0]
R3.All = R3.All %>% filter(!is.na(SchoolName), !is.na(AcademicLevel))

R3.All[Year>.ly,r3 := estBetaParams(r3.rescale,r3.sd),by=.(Home.Suburb,Year,iter,AcademicLevel,SchoolName)]
R3.All[Year>.ly & is.na(r3),r3 := r3.rescale]
R3.All[,r3 := 1/sum(r3)*r3, by = .(Home.Suburb,Year,iter)]

R3.All[Year > .ly & !is.na(r3), mig := rmultinom(1,in.mig,r3),by = .(Home.Suburb,Year,iter)]

R3 = left_join(R3,Mig.suburb.proj[,c(1,2,6)])
R3[Year > .ly, mig := r3.rescale*N.adj]

#store
if(store == T){
  fwrite(R3, "output (2018)/2 in-migrant/In-migration by SA2, School and Level.csv")
  #save(R3.All,file = "output (2018)/2 in-migrant/In-migration by SA2, School and Level (iterations).RData")
}
if(store == T & !exists(".iteration")){
  save(R3.All,file = "output (2018)/2 in-migrant/In-migration by SA2, School and Level (iterations).RData")
}

Mig.in.proj = R3 %>% group_by(SchoolName,AcademicLevel,Year) %>% summarise(mig=sum(mig,na.rm = T))
Mig.in.all = R3.All[,.(sum(mig,na.rm = T)),by=.(SchoolName,AcademicLevel,Year,iter)]
colnames(Mig.in.all)[5] = "mig"

# In-migrant outside ACT ####
MIG.out = transition[AcademicLevel != "00B" & is.na(ACL.pre) & !is.na(Home.Suburb) & HomeRegion=="Outside"&Year>2008,][,.N,by=.(SchoolName,AcademicLevel,Year)]

Mig.out = MIG.out %>% arrange(SchoolName) %>%  pivot_wider(values_from = N,names_from = Year)
Mig.out = Mig.out %>% pivot_longer(c(3:ncol(Mig.out)),names_to = "Year",values_to = "mig.out") %>% as.data.table()
Mig.out[is.na(mig.out),mig.out := 0]

recent.sch = fread("input/recent list from Census (in-sample).csv")
colnames(recent.sch)[3] = "year_open"
Mig.out = left_join(Mig.out,recent.sch)
Mig.out[!is.na(year_open) & Year<year_open,mig.out := NA][,year_open:=NULL]
Mig.out$Year = as.numeric(Mig.out$Year)
Mig.out[Year > (.ly-5),mean := mean(mig.out,na.rm=T),by=.(SchoolName,AcademicLevel)]

list = Mig.out[Year == .ly & mean > 0,.(AcademicLevel,SchoolName)]

Mig.out.proj = left_join(list,Mig.out[,mean:=NULL])

tem = Mig.out.proj %>% filter(Year > (.ly-5)) %>% arrange(desc(Year))
tem = pivot_wider(tem,names_from = Year,values_from = mig.out)
try({ma = apply(tem[,c(7:3)],1,sma,order = 5,h=5)},silent = T)
try({ma = apply(tem[,c(7:3)],1,sma_old,order = 5,h=5)},silent = T)

for (i in 1:length(ma)) {
  x = c(ma[[i]][["forecast"]],rep(ma[[i]][["forecast"]][5],5))
  tmp = bind_cols(tem[i,1:2],mig.out= x,Year = (.ly+1):(.ly+10))
  # Uncertainty: negative binomial here
  Mig.out.proj = bind_rows(Mig.out.proj,tmp)
}

MIG.ALL = full_join(Mig.in.all,Mig.out.proj) %>% as.data.table()
MIG.ALL[is.na(mig),mig := 0]
MIG.ALL[is.na(mig.out),mig.out := 0]
MIG.ALL[Year > .ly,mig.out := rpois(1,mig.out),by = .(SchoolName,AcademicLevel,Year,iter)]
MIG.ALL[,MIG := mig + mig.out]
MIG.ALL = MIG.ALL %>% filter(!(SchoolName=="Good Shepherd Primary School" & AcademicLevel=="Year 7"))
MIG.ALL = MIG.ALL %>% filter(!(SchoolName=="Urambi Primary School"))

Mig.sum = MIG.ALL %>% group_by(SchoolName,AcademicLevel,Year) %>% summarise(m = median(MIG,na.rm = T),q2.5 = quantile(MIG,0.025,na.rm = T),q10 = quantile(MIG,0.1,na.rm = T),q90 = quantile(MIG,0.9,na.rm = T),q97.5 = quantile(MIG,0.975,na.rm = T))

if(store == T){
  save(MIG.ALL,file = "output (2018)/2 in-migrant/In-migration.RData")
  fwrite(Mig.sum, "output (2018)/2 in-migrant/In-migration.csv")
  #save.image(file = "output (2018)/2 in-migrant/2 in-migrant.RData")
}