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

# if (!require("doParallel")){
#   install.packages("doParallel", dep = T)
# }
# 
# registerDoParallel(detectCores()-1)
# iteration = 1000
if(exists(".iteration")){
  iteration = .iteration
}

# code starts from here #
#Loading main data files into R
load("output (2018)/0 data cleaning/cleaned data file.RData")

# R4 ####
#number of transition
TRANS = transition[!is.na(ACL.pre),][,.N,by=.(Year, SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
TRANS = TRANS[,Total := sum(N),by=.(Year, SCH.pre,ACL.pre)][,r4 := N/Total]

# remove new school open year
recent.sch = fread("input/recent list from Census (in-sample).csv")
recent.sch = recent.sch %>% mutate(year_open = Year) %>% select(SchoolName,AcademicLevel,year_open)

R4 = left_join(TRANS,recent.sch, by= c("SCH.pre"="SchoolName","ACL.pre"="AcademicLevel"))
R4[is.na(year_open),year_open:= 2010]

R4[is.na(SchoolName),SchoolName:= "Out"]
R4[is.na(AcademicLevel),AcademicLevel:= "migrant"]

R4.wide = dcast(R4, SCH.pre+ACL.pre+SchoolName+AcademicLevel+year_open ~ Year, value.var = "r4")
R4.wide[,as.character(2012:.ly):=lapply(.SD, function(x) ifelse(is.na(x),0,x)),.SDcols = as.character(2012:.ly)]

R4 = melt(R4.wide,id = 1:5,variable.name = "Year",value.name = "r4")
R4$Year = as.numeric(as.character(R4$Year))
R4[Year<=year_open,r4:=NA][,year_open:=NULL]

R4 = left_join(R4,recent.sch, by= c("SchoolName","AcademicLevel"))
R4[is.na(year_open),year_open:= 2010]
R4[Year<year_open,r4:=NA][,year_open:=NULL]

# extract R4 with at least 5 year of historical data. R4 less than five years will be force to use five-year average
R4.sd5 = R4[Year > (.ly-5) & !is.na(r4) & r4 > 0, .N, by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
R4.sd5 = R4.sd5[N==5&!is.na(SchoolName)]
R4.sd = copy(R4)
R4.sd = left_join(R4.sd,R4.sd5)
R4.sd = copy(R4.sd)
R4.sd[Year > (.ly-7)& N==5, `:=`(`7` = sd(r4,na.rm = T)), by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
R4.sd[Year > (.ly-6)& N==5, `:=`(`6` = sd(r4,na.rm = T)), by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
R4.sd[Year > (.ly-5)& N==5, `:=`(`5` = sd(r4,na.rm = T)), by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
R4.sd[Year > (.ly-4)& N==5, `:=`(`4` = sd(r4,na.rm = T)), by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
R4.sd[Year > (.ly-3)& N==5, `:=`(`3` = sd(r4,na.rm = T)), by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]

tem = R4.sd %>% filter(Year == .ly) %>% pivot_longer(8:12,names_to = "duration",values_to = "R4.sd") %>% group_by(SCH.pre,ACL.pre,SchoolName,AcademicLevel) %>% 
  mutate(R4.sd = ifelse(is.na(R4.sd)&duration==5,0,R4.sd)) %>% slice_min(R4.sd) %>% slice_min(duration) %>% 
  mutate(y = .ly - as.numeric(duration),y2 = .ly - as.numeric(duration)) %>% select(SCH.pre,ACL.pre,SchoolName,AcademicLevel,R4.sd,y,y2)
temx = tem %>% filter(AcademicLevel %in% c("Year 7","Year 11"))
tem = tem %>% filter(!AcademicLevel %in% c("Year 7","Year 11"))

tem2 = R4.sd %>% filter(Year == .ly) %>% pivot_longer(8:12,names_to = "duration",values_to = "R4.sd") %>% group_by(SCH.pre,ACL.pre,SchoolName,AcademicLevel) %>%
  mutate(R4.sd = ifelse(is.na(R4.sd)&duration==5,0,R4.sd)) %>% slice_max(R4.sd) %>% slice_min(duration) %>%
  mutate(y = .ly - as.numeric(duration),y2 = .ly - as.numeric(duration)) %>% select(SCH.pre,ACL.pre,SchoolName,AcademicLevel,R4.sd,y,y2)
tem2 = tem2 %>% filter(AcademicLevel %in% c("Year 7","Year 11"))

tem2$y=temx$y

tem = bind_rows(tem,tem2)
rm(tem2,temx)
setDT(tem)
tem[SchoolName=="Out" & y>(.ly-5), y := (.ly-5)]


R4 = left_join(R4,tem)
R4 = copy(R4)

R4[Year > y, `:=`(R4.mean = mean(r4,na.rm = T)), by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
R4[Year > y2, `:=`(R4.sd = sd(r4,na.rm = T)), by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)][is.na(R4.mean),R4.sd := NA]
# tem = R4[Year > y & r4 == 0, .N, by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
# R4 = left_join(R4,tem)
# R4 = copy(R4)
# R4[Year > y&is.na(N),R4.growth:=lead(r4)-r4,by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
# R4[!is.na(R4.growth), x:=.N, by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
# R4[Year == .ly-1 & x == 1, R4.growth:=NA, by = .(SCH.pre,ACL.pre,SchoolName,AcademicLevel)]
# R4 = R4 %>% group_by(SCH.pre,ACL.pre,SchoolName,AcademicLevel) %>% mutate(R4.growth=mean(R4.growth,na.rm=T))
# setDT(R4)
# R4[is.na(R4.growth),R4.growth:=0][is.na(R4.mean),R4.growth:=NA][,x:=NULL][,N:=NULL]
# R4[,R4.mean := R4.mean+R4.growth]
R4 = left_join(R4,TRANS)
R4[r4==0,N:=0][,y:=NULL][,y2:=NULL]


# examine extreme total R4
examine = R4 %>% filter(Year == .ly) %>% group_by(SCH.pre,ACL.pre) %>% summarise(tot=sum(R4.mean)) %>% arrange(-tot)
View(examine)
examine = examine %>% filter(tot==0)
examine$id = paste(examine$SCH.pre,examine$ACL.pre)

R4.mean = R4[Year == .ly]
## (optional)
R4.mean$id = paste(R4.mean$SCH.pre,R4.mean$ACL.pre)
# R4.mean[,R4.mean.adj:=R4.mean][R4.mean.adj==0,R4.mean.adj:=0.001][,id:= NULL]
R4.mean[,R4.mean.adj:=R4.mean][R4.mean.adj==0 & !id %in% examine$id,R4.mean.adj:=0.0005][,id:= NULL]
#

R4.mean[,R4.mean.adj := 1/sum(R4.mean.adj)*R4.mean.adj, by = .(SCH.pre,ACL.pre)]
R4.mean[is.na(R4.mean.adj),R4.mean.adj := 0]

## project forward
R4.proj =  c()
for (i in (.ly+1):(.ly+10)) {
  tem = R4.mean
  tem$Year = i
  R4.proj= rbind(R4.proj,tem)
}


#ASSUMPTION 3.1 (review each year)
R4.ADJ <- read_excel("assumption/Transition/AS 3.1 R4 adjust (Mar 23).xlsx") %>% as.data.table()
R4.adj = c()
for (i in 1:nrow(R4.ADJ)) {
  for (y in as.numeric(R4.ADJ$year.from[i]):as.numeric(R4.ADJ$year.to[i])) {
    tem = cbind(Year = y,R4.ADJ[i,1:6])
    R4.adj = rbind(R4.adj,tem)
  }
}

R4.ADJ <- read_excel("assumption/Transition/AS 3.1 R4 adjust (in-sample) 2018.xlsx") %>% as.data.table()
R4.adj.2 = c()
for (i in 1:nrow(R4.ADJ)) {
  for (y in as.numeric(R4.ADJ$year.from[i]):as.numeric(R4.ADJ$year.to[i])) {
    tem = cbind(Year = y,R4.ADJ[i,1:6])
    R4.adj.2 = rbind(R4.adj.2,tem)
  }
}
R4.adj.2 = filter(R4.adj.2,Year>.ly)
colnames(R4.adj.2)[6:7] = c("R4.adj.2","R4.reduce.2")
add0 = R4.adj.2 %>% group_by(orig.sch,orig.lvl,Year) %>% summarise(x = sum(R4.adj.2,na.rm = T)) %>% filter(x > 0.99999, x <1.00001)


R4.adj.adj = full_join(R4.adj %>% filter(!is.na(R4.adj)),R4.adj.2%>% filter(!is.na(R4.adj.2)))
R4.adj.adj$R4.adj = ifelse(is.na(R4.adj.adj$R4.adj),R4.adj.adj$R4.adj.2,R4.adj.adj$R4.adj)
R4.adj.adj = select(R4.adj.adj,-R4.adj.2,-R4.reduce.2)
R4.adj.red = full_join(R4.adj %>% filter(!is.na(R4.reduce)),R4.adj.2%>% filter(!is.na(R4.reduce.2)))
R4.adj.red$R4.reduce = ifelse(is.na(R4.adj.red$R4.reduce),R4.adj.red$R4.reduce.2,R4.adj.red$R4.reduce)
R4.adj.red = select(R4.adj.red,-R4.adj.2,-R4.reduce.2)
R4.adj = rbind(R4.adj.adj,R4.adj.red)

R4.adj = R4.adj %>% as.data.frame()
R4.adj = R4.adj[which(!duplicated(R4.adj[ , c("Year","orig.lvl","orig.sch","dest.lvl","dest.sch")])),]

R4.adj = R4.adj %>% filter(Year <= .ly+10) %>% setDT()

R4.mean.adj = full_join(R4.proj,R4.adj[!is.na(R4.adj)]%>% select(-R4.reduce),by=c("Year","SCH.pre"="orig.sch","ACL.pre"="orig.lvl","SchoolName"="dest.sch","AcademicLevel"="dest.lvl"))
R4.mean.adj = full_join(R4.mean.adj,R4.adj[!is.na(R4.reduce)]%>% select(-R4.adj),by=c("Year","SCH.pre"="orig.sch","ACL.pre"="orig.lvl","SchoolName"="dest.sch","AcademicLevel"="dest.lvl"))
R4.mean.adj[!is.na(R4.reduce),R4.adj := R4.adj-R4.reduce][,R4.reduce := NULL]
examine = R4.mean.adj[!is.na(R4.adj)&!is.na(R4.mean.adj)]
View(examine)

R4.mean.adj[!is.na(R4.adj),R4.mean.adj := R4.adj]
# add 0 to adjustment if we have 100 percent destination
for (row in 1:nrow(add0)) {
  R4.mean.adj[SCH.pre == add0$orig.sch[row]&ACL.pre == add0$orig.lvl[row]&Year ==add0$Year[row]&is.na(R4.adj), R4.mean.adj := 0]
  R4.mean.adj[SCH.pre == add0$orig.sch[row]&ACL.pre == add0$orig.lvl[row]&Year ==add0$Year[row]&is.na(R4.adj), R4.adj := 0]
}
# examine extreme total R4
examine = R4.mean.adj %>% group_by(SCH.pre,ACL.pre,Year) %>% summarise(tot=sum(R4.mean.adj)) %>% filter(tot < 0.99999 |tot >1.00001) %>% arrange(-tot)
View(examine)

examine$id = paste(examine$Year,examine$SCH.pre,examine$ACL.pre,sep = "_")
R4.mean.adj$id = paste(R4.mean.adj$Year,R4.mean.adj$SCH.pre,R4.mean.adj$ACL.pre,sep = "_")
for (i in examine$id) {
  tem = R4.mean.adj %>% filter(id==i,SchoolName=="Out")
  tem$Year = as.character(tem$Year)
  if(nrow(tem)==0){
    x = strsplit(i, split = "_")[[1]]
    names(x) = c("Year","SCH.pre","ACL.pre")
    tem= bind_rows(tem,x)
    tem$SchoolName = "Out"
    tem$AcademicLevel = "migrant"
    
    tem2 = R4.mean.adj %>% filter(id==i) %>% summarise(t = sum(R4.mean.adj)) %>% pull(t)
    if (tem2 >= 1) {
      tem$R4.mean.adj = 0.04
    }else{
      tem$R4.mean.adj = 1-tem2
    }
    tem$Year = as.numeric(tem$Year)
    R4.mean.adj = bind_rows(R4.mean.adj,tem)
  }
}

# examine extreme total R4
examine = R4.mean.adj %>% group_by(SCH.pre,ACL.pre,Year) %>% summarise(tot=sum(R4.mean.adj)) %>% filter(tot < 0.99999 |tot >1.00001) %>% arrange(-tot)
View(examine)

R4.mean.adj = R4.mean.adj %>% filter(!(SchoolName=="Urambi Primary School"))
R4.mean.adj = R4.mean.adj %>% filter(!(SchoolName=="Good Shepherd Primary School"& AcademicLevel=="Year 7"))

R4.mean.adj[,R4.mean.adj := 1/sum(R4.mean.adj,na.rm = T)*R4.mean.adj, by =  .(Year,SCH.pre,ACL.pre)][,id:= NULL]
R4.mean.adj = copy(R4.mean.adj)
R4.mean.adj[,R4.sum := sum(R4.mean.adj,na.rm = T),by =  .(Year,SCH.pre,ACL.pre)]
R4.mean.adj[is.na(R4.mean.adj),R4.mean.adj := 0]
R4.mean.adj[SchoolName=="Out"&is.na(R4.sum),R4.mean.adj := 1][,R4.sum:=NULL]

R4.all = bind_rows(R4,R4.mean.adj)
R4.all[Year>.ly,r4:=R4.mean.adj]
R4.all[Year>.ly,N:=NA]
R4.all[Year>.ly,Total:=NA][,R4.mean.adj:=NULL]
m = R4.mean %>% filter(R4.mean>0) %>% summarise(x = mean(R4.sd,na.rm = T)) %>% pull(x)
R4.all[Year>.ly & is.na(R4.sd),R4.sd := m]

### ad hoc adjustment!
R4.all[Year>.ly & AcademicLevel %in% c("Year 7","Year 11"),R4.sd := R4.sd*1.5]
for (i in 1:9) {
  R4.all[Year==(.ly+1+i),R4.sd := R4.sd*(1.08^i)]
}


if(store == T){
  R4.all = R4.all %>% arrange(SCH.pre,ACL.pre,SchoolName,AcademicLevel)
  fwrite(R4.all, "output (2018)/3 transition/R4 all.csv")
}
