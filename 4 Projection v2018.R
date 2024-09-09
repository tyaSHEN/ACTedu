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

iteration = 100
if(exists(".iteration")){
  iteration = .iteration
}

# code starts from here #
#Loading main data files into R
load("output (2018)/0 data cleaning/cleaned data file.RData")
load("output (2018)/1 preschool/School entries.RData")
load("output (2018)/2 in-migrant/In-migration.RData")
r4 = fread("output (2018)/3 transition/R4 all.csv")

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

#ASSUMPTION 4.1 extra in-migrant to new preschool
in.00B <- read_excel("assumption/Projection/AS 4.1 add migration to new 00B (in-sample) 2018.xlsx")
in.00B = left_join(in.00B,MIG.ALL[,c(1,2,3,4,7)])
in.00B$extra = in.00B$MIG * in.00B$ratio
setDT(in.00B)
#
registerDoParallel(min(detectCores(),12))
RES = foreach(i=1:iteration,.packages = c("tidyverse","data.table","readxl")) %dopar% { 
  
  # project first year ####
  ## first year enrollment ####
  #number of transition
  Base = record[Year == .ly][,.N,by=.(SchoolName,AcademicLevel)]
  
  #ASSUMPTION 4.0 change 00B (2022 only)
  if(.ly==2099){
    B2022 = Base[AcademicLevel=="00B"]
    Base = Base[AcademicLevel!="00B"]
    B2021 <- read_excel("assumption/Projection/AS 4.0 change 00B based in 2023 (Jan 23).xlsx")
    B202X = full_join(B2021,B2022,by=c("SchoolName","AcademicLevel"))
    B202X = cbind(B202X,x= rbinom(nrow(B202X), 1, 0.85))
    B202X$N = ifelse(B202X$x == 1,B202X$N.x,B202X$N.y)
    B202X$N = ifelse(is.na(B202X$N),0,B202X$N)
    B202X = B202X[c(1,2,6)]
    Base = bind_rows(B202X,Base)
  }
  #
  
  Base$Year = .ly+1
  
  ## transition ####
  r4.subset = r4[Year == .ly+1,.(SCH.pre,ACL.pre,SchoolName,AcademicLevel,Year,r4,R4.sd)]
  
  r4.subset[Year>.ly,r4:= estBetaParams(r4,R4.sd),by=.(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
  # r4.subset[Year>.ly & is.na(r4.random),r4.random := r4]
  r4.subset[,r4 := 1/sum(r4)*r4, by = .(SCH.pre,ACL.pre)]
  
  NEX = full_join(Base,r4.subset,by = c("Year","SchoolName"="SCH.pre","AcademicLevel"= "ACL.pre")) %>% as.data.table()
  NEX = NEX[!is.na(N)]
  NEX$r4 = ifelse(is.na(NEX$r4)&NEX$AcademicLevel.y == "migrant",1,NEX$r4)
  NEX = NEX[!is.na(r4)]
  NEX[,trans := rmultinom(1,round(N),r4),by=.(SchoolName,AcademicLevel)][,N:=NULL][,r4:=NULL][,R4.sd:=NULL]
  nex = NEX %>% group_by(Year,SchoolName.y,AcademicLevel.y) %>% summarise(trans = sum(trans))
  
  ## in-migrant ####
  inmig.subset = MIG.ALL[Year == .ly+1 & iter == i,.(SchoolName,AcademicLevel,Year,MIG)]
  colnames(inmig.subset)[4] = "Entries"
  
  ## preschool ####
  preschool.subset = Entries[Year == .ly+1 & iter == i,.(SchoolName,Year,Entries)]
  preschool.subset = left_join(preschool.subset, in.00B[iter == i,c(1,3,7)])
  preschool.subset[!is.na(extra),Entries := Entries+extra][,extra := NULL]
  preschool.subset$AcademicLevel= "00B"
  
  addition = bind_rows(inmig.subset,preschool.subset)
  addition = addition[!is.na(SchoolName)]
  
  nex = full_join(addition,nex,by = c("Year","SchoolName"="SchoolName.y","AcademicLevel"= "AcademicLevel.y"))
  nex[is.na(Entries),Entries:=0]
  nex[is.na(trans),trans:=0]
  nex = nex %>% filter(SchoolName!="Out") %>% select(-Year)
  nex$enrol = nex$Entries+nex$trans
  
  ENROL = nex[,c(1,2,5)]
  
  colnames(addition) = c("SchoolName.y","AcademicLevel.y","Year","trans")
  NEX = bind_rows(NEX,addition)
  
  # project future years ####
  for (y in (.ly+2):(.ly+10)) {
    
    ## base population ####
    Base = ENROL %>% select(c(1,2,ncol(ENROL)))
    Base$Year = y
    colnames(Base)[3] = "enrol"
    
    ## transition ####
    r4.subset = r4[Year == y,.(SCH.pre,ACL.pre,SchoolName,AcademicLevel,Year,r4,R4.sd)]
    
    r4.subset[Year>.ly,r4:= estBetaParams(r4,R4.sd),by=.(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
    # r4.subset[Year>.ly & is.na(r4.random),r4.random := r4]
    r4.subset[,r4 := 1/sum(r4)*r4, by = .(SCH.pre,ACL.pre)]
    
    tem = full_join(Base,r4.subset,by = c("Year","SchoolName"="SCH.pre","AcademicLevel"= "ACL.pre"))
    tem = tem[!is.na(enrol)]
    tem$r4 = ifelse(is.na(tem$r4)&tem$AcademicLevel.y == "migrant",1,tem$r4)
    tem = tem[!is.na(r4)]
    # tem = tem[!is.na(SchoolName)]
    tem[,trans := rmultinom(1,round(enrol),r4),by=.(SchoolName,AcademicLevel)][,enrol:=NULL][,r4:=NULL][,R4.sd := NULL]
    NEX = bind_rows(NEX,tem)
    nex = tem %>% group_by(Year,SchoolName.y,AcademicLevel.y) %>% summarise(trans = sum(trans))
    
    ## preschool ####
    preschool.subset = Entries[Year == y & iter == i,.(SchoolName,Year,Entries)]
    preschool.subset = left_join(preschool.subset, in.00B[iter == i,c(1,3,7)])
    preschool.subset[!is.na(extra),Entries := Entries+extra][,extra := NULL]
    preschool.subset$AcademicLevel= "00B"
    
    ## in-migrant ####
    inmig.subset = MIG.ALL[Year == y & iter == i,.(SchoolName,AcademicLevel,Year,MIG)]
    colnames(inmig.subset)[4] = "Entries"
    
    addition = bind_rows(inmig.subset,preschool.subset)
    addition = addition[!is.na(SchoolName)]
    
    nex = full_join(addition,nex,by = c("Year","SchoolName"="SchoolName.y","AcademicLevel"= "AcademicLevel.y"))
    nex[is.na(Entries),Entries:=0]
    nex[is.na(trans),trans:=0]
    nex = nex %>% filter(SchoolName!="Out") %>% select(-Year)
    
    nex$enrol = nex$Entries+nex$trans
    
    ENROL = full_join(ENROL,nex[,c(1,2,5)],by = c("SchoolName", "AcademicLevel"))
    
    colnames(addition) = c("SchoolName.y","AcademicLevel.y","Year","trans")
    NEX = bind_rows(NEX,addition)
    
  }
  
  colnames(ENROL)= c("SchoolName", "AcademicLevel",(.ly+1):(.ly+10))
  
  NEX[SchoolName.y=="Out",`:=`(SchoolName.y=NA,AcademicLevel.y=NA)]
  
  RES=list(ENROL=ENROL,COMP=NEX)
  
  RES
  
}
stopImplicitCluster()

Enrolment = c()
# Component = c()
Component = list()
for(i in 1:length(RES)){
  tem = RES[[i]]$ENROL
  tem$iter= i
  Enrolment = rbind(Enrolment,tem)
  Component[[i]] = RES[[i]]$COMP %>% filter(trans>0)
  # tem = RES[[i]]$COMP %>% pivot_wider(names_from = "Year",values_from = "trans") %>% mutate(iter = i)
  # Component = rbind(Component,tem)
}

# summary of the results
Enrolment.sum = Enrolment %>% pivot_longer(3:12,names_to = "Year",values_to = "Enrolment") %>% 
  group_by(SchoolName,AcademicLevel,Year) %>% summarise(mean = mean(Enrolment,na.rm = T),median = median(Enrolment,na.rm = T),lower80=quantile(Enrolment,0.1,na.rm=T),upper80=quantile(Enrolment,0.9,na.rm=T),
                                                        lower95=quantile(Enrolment,0.025,na.rm=T),upper95=quantile(Enrolment,0.975,na.rm=T))
Hist = record[,.N,by=.(Year,SchoolName,AcademicLevel)]
Hist[,median := N][,N:=NULL]
Hist[Year == .ly,`:=`(lower80 = median,upper80 = median,lower95 = median,upper95 = median)]
Enrolment.sum$Year=as.numeric(Enrolment.sum$Year)
Enrolment.sum = bind_rows(Enrolment.sum,Hist)
school_detail <- read_csv("input/school_details_year_level.csv")
Enrolment.sum = left_join(Enrolment.sum,school_detail)
# ACT total
Enrolment %>% pivot_longer(3:12,names_to = "Year",values_to = "Enrol") %>% 
  group_by(Year,iter) %>% summarise(N= sum(Enrol,na.rm = T)) %>% summarise(median(N),quantile(N,0.025),quantile(N,0.975)) %>% View()

if(store == T){
  fwrite(Enrolment.sum, "output (2018)/Enrolment by school and level.csv")
  save(Component,file=paste0("output (2018)/Component (iterations).RData"))
  save(Enrolment,file=paste0("output (2018)/Enrolment (iterations).RData"))
}

