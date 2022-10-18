
                                                     ###########################
                                                     ###     Model Data      ###
                                                     ###########################

#==================================================================================================================================
# Aim    : Main Model Fitting
# Project: Appalachians
# Date   : 14 Nov. 2020
# What?  : Manually edited distance to office when it varied through time
#		   Solve the merged site pbm in GIS before re-running the extraction code
#
# Output: "DataNoTime.csv""and "DataPanel.csv"
#         And all the model fitting data and graphs that are in the "SI-StepbyStep" file (in the "__Writings" folder)
#==================================================================================================================================


setwd('C:/Users/Diane_LeBouille/Documents/01_Thesis Work/2019_Appalachians/Model')
DataBuff1 = read.table(file='_GISBufferData1K.txt', header=TRUE, sep=",", na.strings='NA')
DataBuff5 = read.table(file='_GISBufferData5K.txt', header=TRUE, sep=",", na.strings='NA')
DataBuff10 = read.table(file='_GISBufferData10K.txt', header=TRUE, sep=",", na.strings='NA')
DataSurv = read.table(file='_SurveyData.txt', header=TRUE, sep="\t", na.strings='NA')
DataActi = read.table(file='_ActivityData.txt', header=TRUE, sep="\t", na.strings='NA')
DataFire = read.table(file='_FireData.txt', header=TRUE, sep="\t", na.strings='NA')
DataOff = read.table(file='_OfficeData.txt', header=TRUE, sep="\t", na.strings='NA')
#==================================================
# Note on the txt files (all just c/c from spredsheets in "__Stored Data (read from txt).xlsx")
# _BufferData -> This data is the output from running the a Python script which calculate parameter values within buffers
# _SurveyData -> This data comes from the surveys (record of spendings, trips made, contracts signed and staff salary estimates)
# _ActivityData -> This data is aggregation of all land purchases made by TNC for the years and states in this study
# _Fire Data -> This data comes from the surveys too (prescribed fire: yes or no for a given year on a given site)
#
# NB: Dates and Areas values have been checked across shapefiles and survey data, there should be no more discrepancies
#     Area values in the survey data are rounded and dates are adjusted (ex. P.A. bought in Dec 2001 would be marked as bought in 2002)
#     -> for this reason, dates and areas used will be the ones from the surveys
#==================================================




#===============================
# DATA - FORMAT, PRE-SHAPING
#===============================


# Straightening up formats and such:
#-----------------------------------
DataBuff = DataBuff5 # NB: don't forget to change the names of csv outputs below accordingly, too!
DataBuff$SiteID = as.character(DataBuff$SiteID)
DataBuff$AreaAcre = 0.00404686 * DataBuff$AreaAcre #(1 acre = 0.00404686 km²)
colnames(DataBuff)[colnames(DataBuff)=="AreaAcre"] = "AreaSite"
DataBuff$ProtYear =  format(as.Date(paste0(DataBuff$ProtYear, '-01-01'), "%Y-%m-%d"), "%Y")
DataBuff$Date = format(as.Date(DataBuff$Date, "%Y-%m-%d %H:%M:%S"), "%Y")
head(DataBuff)

DataSurv$PBRS = as.character(DataSurv$PBRS)
DataSurv$DistOff = as.numeric(DataSurv$DistOff)
DataSurv$YearAcq = format(as.Date(paste0(DataSurv$YearAcq, '-01-01'), "%Y-%m-%d"), "%Y")
DataSurv$Area = DataSurv$Area * 0.00404686 #(1 acre = 0.00404686 km²)
head(DataSurv)

DataActi$Tract_ID = as.factor(DataActi$Tract_ID)
DataActi$TransDate = format(as.Date(DataActi$TransDate,"%m/%d/%y"),"%Y")
head(DataActi)

DataFire$ID = as.character(DataFire$ID)
DataFire$FYr = format(as.Date(paste0(DataFire$FYr, '-01-01'), "%Y-%m-%d"), "%Y")

DataOff$ID = as.character(DataOff$ID)
DataOff$YrChange = format(as.Date(paste0(DataOff$YrChange, '-01-01'), "%Y-%m-%d"), "%Y")


"As it is right now, CPI correction was already applied in the excel spreadsheets made from the surveys, but Purchase Price, FMV and Total Costs still needed it "
# CPI correction of acquisition related costs:
#---------------------------------------------
CPI = c(1.37,1.34,1.32,1.29,1.25,1.21,1.17,1.14,1.10,1.10,1.09,1.05,1.03,1.02,1.00)
for (j in 23:25){
	for (i in 1:dim(DataSurv)[1]){
		DataSurv[i,j] = round((DataSurv[i,j] * CPI[as.numeric(DataSurv$YearAcq[i])-1999]), 2)
	}
}
head(DataSurv)


# Computing Activity metrics:
#----------------------------
USstates = c("AL","GA","MD","NC","PA","SC","TN","VA","WV")
Years = c(2000:2014)
Activity = matrix(data=NA,nrow=length(Years),ncol=length(USstates),dimnames=list(paste(Years,'Act', sep=''),USstates))
for (s in USstates){
	for (y in Years){
		ChapYr = DataActi[which(DataActi$State_1==s & DataActi$TransDate==y),]
		Activity[paste(y,'Act', sep=''),s] = sum(ChapYr$Pprice)
    }
}
Activity


# All PAs and EAS in the buffer that have no dates are set to 2000 + get rid of duplicates (protected in a same year)
#--------------------------------------------------------------------------------------------------------------------
BufProtected = NULL
IDs = unique(DataBuff$SiteID)
ProtecMissingYears = sum(is.na(DataBuff$ProtYear))
print(paste("Number of missing acquisition dates for PAs and Easments is ", ProtecMissingYears, " out of ", length(DataBuff$ProtYear)))
# 44 out of 152 for 1k buffers / 59 out of 332 for 5k buffers / 116 out 770 for 10k buffers
DataBuff$ProtYear[which(DataBuff$ProtYear<2000 | is.na(DataBuff$ProtYear))] = "2000"
for (i in IDs){
	for (t in c('EAS','PA')){
		site = DataBuff[which(DataBuff$SiteID==i & DataBuff$ProtectionType==t),]
		BufProtected = rbind(BufProtected, site[!rev(duplicated(rev(site$ProtYear))),])
	}
}
head(BufProtected)
DataBuff = BufProtected


# Adding the non-time-varying factors, losing the costs:
#-------------------------------------------------------
# (time-varying factor are, Fire, DistOffice and Activity - We will re-add costs later, one per line, in the long format)
# Added: BufferArea, AgriArea, RoadLength, UAsArea, RugosityMean, RugositySD and [ProtectionType, CumulArea and ProtYear]no-time

DataNoTime = DataSurv
D = dim(DataNoTime)[1]
DataNoTime = cbind(DataNoTime,
	BufferArea=rep(NA, D), AgriArea=rep(NA, D), RoadLength=rep(NA, D), UAsArea=rep(NA, D), RugosityMean=rep(NA, D), RugositySD=rep(NA, D), PAsEAS=rep(NA, D),
	AvgCost=rep(NA, D), VarCost=rep(NA, D), AvgCostPerKm2=rep(NA, D), VarCostPerKm2=rep(NA, D), SDCostPerKm2=rep(NA, D), TSP=rep(NA, D))
for (i in 1:D){
	Costs = DataNoTime[i,8:22][!is.na(DataNoTime[i,8:22])] #Keeps non-NA cost entries
	DataNoTime$TSP[i] = length(Costs) #Time Since Protection
	DataNoTime$AvgCost[i] = mean(Costs) # Mean and Var costs (across all years between acquisition and end of study)
	DataNoTime$VarCost[i] = var(Costs)
	CostPerKm2 = Costs/DataNoTime$Area[i]
	DataNoTime$AvgCostPerKm2[i] = mean(CostPerKm2) # Mean, Var and SD of Costs per Km² (for stats later down, not for model)
	DataNoTime$VarCostPerKm2[i] = var(CostPerKm2)
	DataNoTime$SDCostPerKm2[i] = sd(CostPerKm2)
	# Fetch the rest from DataBuff:
	PBRS_Site = DataBuff[which(DataBuff$SiteID==DataNoTime$PBRS[i]),]
	DataNoTime$BufferArea[i] = PBRS_Site$BufferArea[1]
	DataNoTime$AgriArea[i] = PBRS_Site$AgriArea[1]
	DataNoTime$RoadLength[i] = PBRS_Site$RoadLength[1]
	DataNoTime$UAsArea[i] = PBRS_Site$UAsArea[1]
	DataNoTime$RugosityMean[i] = PBRS_Site$RugosityMean[1]
	DataNoTime$RugositySD[i] = PBRS_Site$RugositySD[1]
	CumulEAS = tail(PBRS_Site$CumulArea[which(PBRS_Site$ProtectionType=='EAS')], 1)
	CumulPAs = tail(PBRS_Site$CumulArea[which(PBRS_Site$ProtectionType=='PA')], 1)
	DataNoTime$PAsEAS[i] = sum(CumulEAS, CumulPAs)
}
CostDataSaved = cbind(PBRS=DataNoTime$PBRS, DataNoTime[,c(8:22)])
DataNoTime = DataNoTime[,-c(8:22)]
head(DataNoTime)
write.csv(DataNoTime, "DataNoTime5.csv", row.names = FALSE)


# Long panel format, one year per line (allows for time-varying factors to be added)
#-----------------------------------------------------------------------------------
DataPanel = NULL
DataNoTime = cbind(DataNoTime, Costs=rep(NA, D), YrSinceProt=rep(NA, D), Year=rep(NA, D), ChapterActivity=rep(NA, D), FireYr=rep(NA, D))
OfficeChanges = unique(DataOff$ID)
for (i in 1:dim(DataNoTime)[1]){
	PAsEAS = NULL
	DataTemp = NULL
	TSP=DataNoTime$TSP[i]
	DataTemp = DataNoTime[rep(i, TSP), ] # rbind the first row TSP times
	DataTemp$YrSinceProt = c(1:TSP)
	Year = as.numeric(DataNoTime$YearAcq[i])+c(0:(TSP-1))
	DataTemp$Year = Year
	DataTemp$ChapterActivity = Activity[paste(Year,'Act', sep=''), DataNoTime$State[i]]
	DataTemp$FireYr = rep("N", TSP)
	costRaw = which(CostDataSaved$PBRS==DataNoTime$PBRS[i])
	costNotNA = c(FALSE,!is.na(CostDataSaved[costRaw, 2:dim(CostDataSaved)[2]]))
	DataTemp$Costs = t(CostDataSaved[costRaw,costNotNA])
	for (y in Year){
		EASsoFar = tail(DataBuff$CumulArea[which(DataBuff$SiteID==DataNoTime$PBRS[i] & DataBuff$ProtectionType=='EAS' & DataBuff$ProtYear<=y)], 1)
		PAsoFar = tail(DataBuff$CumulArea[which(DataBuff$SiteID==DataNoTime$PBRS[i] & DataBuff$ProtectionType=='PA' & DataBuff$ProtYear<=y)], 1)
		PAsEAS = c(PAsEAS,sum(EASsoFar,PAsoFar))
	}
	DataTemp$PAsEAS = PAsEAS
	if (DataNoTime$Fire[i] == "Y"){
		whenF = DataFire$FYr[which(DataFire$ID == DataNoTime$PBRS[i])]
		DataTemp$FireYr[match(whenF, Year)]= "Y"
	}
	if (is.element(DataNoTime$PBRS[i], OfficeChanges)){
		whichOC = which(DataOff$ID == DataNoTime$PBRS[i])
		whenOC = DataOff$YrChange[whichOC]
		DataTemp$DistOff[DataTemp$Year < whenOC] = DataOff$OffDist1[whichOC]
		DataTemp$DistOff[DataTemp$Year >= whenOC] = DataOff$OffDist2[whichOC]
	}
	DataPanel = rbind(DataPanel,DataTemp)
}
DataPanel$DistOff[which(DataPanel$PBRS=="1088138599133" & DataPanel$FireYr=="Y")] = 142
#NB: when  Warm_Spring_Mtn1 (1088138599133) is on a FireYr = "Y", then DistOff = 142 (people coming from both offices, in 1/3-2/3 proportions)
head(DataPanel)
DataPanel[which(DataPanel$PBRS=="1088138599133"),] # Just to check ^^
write.csv(DataPanel, "DataPanel5.csv", row.names = FALSE)




#===============================
# PRE-TESTS
#===============================
setwd('C:/Users/Diane_LeBouille/Documents/01_Thesis Work/2019_Appalachians/Model')
DataNoTime = read.table(file = "DataNoTime5.csv", header=TRUE, sep=",", na.strings='NA')
DataPanel = read.table(file="DataPanel5.csv", header=TRUE, sep=",", na.strings='NA')


DataNoTime$PBRS = as.character(DataNoTime$PBRS)
DataNoTime$YearAcq = as.numeric(DataNoTime$YearAcq)
DataNoTime$TSP = as.numeric(DataNoTime$TSP)
DataNoTime$AgriArea = DataNoTime$AgriArea/DataNoTime$BufferArea
DataNoTime$PAsEAS = DataNoTime$PAsEAS/DataNoTime$BufferArea
Visitability = NULL
Visitability = (((DataNoTime$UAsArea/DataNoTime$BufferArea)+1)*DataNoTime$RoadLength)
DataNoTime = cbind(DataNoTime, Visitability)
DataNoTime$BufferArea = NULL
DataNoTime$RoadLength = NULL
DataNoTime$UAsArea = NULL

DataPanel$PBRS = as.character(DataPanel$PBRS)
DataPanel$YearAcq = as.numeric(DataPanel$YearAcq)
DataPanel$YrSinceProt = as.numeric(DataPanel$YrSinceProt)
DataPanel$Year = as.numeric(DataPanel$Year)
DataPanel$AgriArea = DataPanel$AgriArea/DataPanel$BufferArea
DataPanel$PAsEAS = DataPanel$PAsEAS/DataPanel$BufferArea
Visitability = NULL
Visitability = (((DataPanel$UAsArea/DataPanel$BufferArea)+1)*DataPanel$RoadLength)
DataPanel = cbind(DataPanel, Visitability)
DataPanel$BufferArea = NULL
DataPanel$RoadLength = NULL
DataPanel$UAsArea = NULL

head(DataNoTime)
head(DataPanel)


### Couple exploratory graphs
#----------------------------

par(mfrow=c(3,1)) #Cost distribution graphs
hist(DataPanel$Costs, breaks=100, prob = TRUE, main="Histogram - Costs")
lines(density(DataPanel$Costs))
hist(log(DataPanel$Costs+1), breaks=30, prob = TRUE, main="Histogram - Log-transformed (+1)")
lines(density(log(DataPanel$Costs+1)))
hist((DataPanel$Costs)^(1/3), breaks=30, prob = TRUE, main="Histogram - Cuberoot-transformed")
lines(density((DataPanel$Costs)^(1/3)))

par(mfrow=c(1,2), cex.axis=0.7, cex.main=1.2, cex.lab=1) #Couple other stats about the data - Part1
barplot(table(DataNoTime$YearAcq), main="Nb of Acquisition per year\nbetween 2000 and 2014", ylab="Nb of acquisitions", xlab="years")
barplot(table(DataNoTime$TSP), main="Distibution of protection duration\nas of 2014", ylab="years", xlab="time since protection")

par(mfrow=c(1,3), cex.axis=0.7, cex.main=1.2, cex.lab=1) #Couple other stats about the data - Part2
barplot(table(DataNoTime$State), main="Nb of acquisition per State", ylab="Nb of acquisitions", xlab="state")
AreaState = tapply(DataNoTime$Area, DataNoTime$State, sum)
barplot(AreaState, main="Area acquired per State", ylab="area acquired (km2)", xlab="state")
CostState = tapply(DataNoTime$TotCost, DataNoTime$State, sum)
barplot(CostState, main="Total acquisition spending\nper State", ylab="total acquisition cost ($)", xlab="state")

plot.ts(Activity, main="Chapter Activity per year") #Couple other stats about the data - Part3

boxplot(DataNoTime$AvgCost ~ DataNoTime$State, xlab = "State", ylab = "Avg Cost per Year", main = "Avg yearly costs across States")
boxplot(log(DataNoTime$AvgCost) ~ DataNoTime$State, xlab = "State", ylab = "Avg Cost per Year", main = "Avg yearly costs across States")


# Descriptive Stats for the article
quantile(DataPanel$Costs, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$Area, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$Elevation, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$RugosityMean, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$PP, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$DistOff, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$AgriArea, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$PAsEAS, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataNoTime$Visitability, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
quantile(DataPanel$ChapterActivity, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)

table(DataPanel$FireYr)
table(DataNoTime$Fire)
table(DataPanel$YrSinceProt)
table(DataNoTime$YearAcq)
table(DataNoTime$MngedFor)
table(DataNoTime$IdealCond)
table(DataNoTime$F_type)

quantile((DataPanel$Costs/DataPanel$Area), probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE) # distri of $/km² per site x year
quantile(DataNoTime$AvgCostPerKm2, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE) # distri of the averaged spendings/km² PER SITE (across all years)
quantile(DataNoTime$VarCostPerKm2, probs=seq(0, 1, 0.25), na.rm=TRUE, names=TRUE)
CVSite=DataNoTime$SDCostPerKm2/DataNoTime$AvgCostPerKm2 # coefficient of variation
median(CVSite)
range(DataPanel$Costs)


### Looking at the variables for outliers (and the need for log-transfo)
#-----------------------------------------------------------------------

par(mfrow=c(4,3), cex.axis=0.7, cex.main=1.2, cex.lab=1) #Explanatory variables distribution
hist(DataNoTime$Area,breaks="FD",main="Site Area", ylab="", xlab="Area (km2)")
hist(DataNoTime$PP,breaks="FD", main="Cost of Acquisition", ylab="", xlab="total aquisition cost ($)")
hist(DataPanel$ChapterActivity,breaks="FD", main="Chapter Budget", ylab="", xlab="Money spent per chapter per year")
hist(DataNoTime$Elevation,breaks="FD", main="Site Elevation", ylab="", xlab="altitude (m)")
hist(DataNoTime$RugosityMean,breaks="FD", main="Site Rugosity", ylab="", xlab="rugosity index")
hist(DataNoTime$DistOff, breaks=10, main="Distance from TNC office", ylab="", xlab="Distance from Managing Office (km)")
hist(DataNoTime$AgriArea,breaks="FD",main="Proprotion of\nagricultural land within 5km", ylab="", xlab="agri. land proportion")
hist(DataNoTime$PAsEAS,breaks="FD",main="Proprotion of\nprotected land within 5km", ylab="", xlab="prot. land proportion")
hist(DataNoTime$Visitability,breaks="FD",main="Visitability Index", ylab="", xlab="(urban area+1)*road length")
plot(DataNoTime$Benef,main="Benef", ylab="", xlab="Benef")
hist(DataNoTime$BenefFrag,breaks="FD",main="BenefFrag", ylab="", xlab="Benef - Fragmentation")
hist(DataNoTime$BenefVert,breaks="FD",main="BenefVert", ylab="", xlab="Benef - Vertebr. Sp.")

par(mfrow=c(5, 2), mar=c(3, 3, 3, 1))	#Cleveland plots
dotchart(DataPanel$Costs, main="Management Costs", groups=DataPanel$State)
dotchart(DataPanel$Area, main="Area", groups=DataPanel$State)
dotchart(DataPanel$PP, main="Cost of Acquisition", groups=DataPanel$State)
dotchart(DataPanel$ChapterActivity, main="Chapter Activity", groups=DataPanel$State)
dotchart(DataPanel$Elevation, main="Elevation", groups=DataPanel$State)
dotchart(DataPanel$RugosityMean, main="Rugosity", groups=DataPanel$State)
dotchart(DataPanel$DistOff, main="Distance from TNC Office", groups=DataPanel$State)
dotchart(DataPanel$AgriArea, main="Proprotion of\nagricultural land within 5km", groups=DataPanel$State)
dotchart(DataPanel$PAsEAS, main="Proprotion of\nprotected land within 5km", groups=DataPanel$State)
dotchart(DataPanel$Visitability, main="Visitability Index", groups=DataPanel$State)

# ==> Money is very skewed, and Visitability has an outlier, apart from that it's not bad.

DataLog = DataPanel
DataLog$Costs = log(DataLog$Costs+1)
DataLog$Area = log(DataLog$Area)
DataLog$ChapterActivity = log(DataLog$ChapterActivity+1)
DataLog$PP = log(DataLog$PP+1)
DataLog$Year = as.factor(DataLog$Year)
DataLog$YearAcq = as.factor(DataLog$YearAcq)
DataLog$FireYr = as.factor(DataLog$FireYr)
DataLog$RugosityMean = DataLog$RugosityMean*1000 # scaling
DataLog$IdealCond = as.factor(DataLog$IdealCond)
DataLog$MngedFor = as.factor(DataLog$MngedFor)
DataLog$Benef = as.factor(DataLog$Benef)
DataLog$F_type = as.factor(DataLog$F_type)
DataLog$Invasive = as.factor(DataLog$Invasive)
head(DataLog)


par(mfrow=c(4,3), cex.axis=0.7, cex.main=1.2, cex.lab=1) #Explanatory variables distribution again, after some log transfos
hist(DataLog$Area, breaks="FD", main="log(Site Area)", ylab="", xlab="Area (km2)")
hist(DataLog$PP, breaks="FD", main="log(Cost of Acquisition)", ylab="", xlab="total aquisition cost ($)")
hist(DataLog$ChapterActivity, breaks="FD", main="log(Chapter Budget)", ylab="", xlab="Money spent per chapter per year")
hist(DataLog$Elevation, breaks="FD", main="Site Elevation", ylab="", xlab="altitude (m)")
hist(DataLog$RugosityMean, breaks="FD", main="Site Rugosity", ylab="", xlab="rugosity index")
hist(DataLog$DistOff, breaks=10, main="Distance from TNC office", ylab="", xlab="Distance from Managing Office (km)")
hist(DataLog$AgriArea, breaks="FD", main="Proprotion of\nagricultural land within 5km", ylab="", xlab="agri. land proportion")
hist(DataLog$PAsEAS, breaks="FD", main="Proprotion of\nprotected land within 5km", ylab="", xlab="prot. land proportion")
hist(DataLog$Visitability,breaks="FD", main="Visitability Index", ylab="", xlab="(urban area+1)*road length")
plot(DataLog$Benef, breaks="FD", main="Benef", ylab="", xlab="Benef")
hist(DataLog$BenefFrag, breaks="FD", main="BenefFrag", ylab="", xlab="Benef - Fragmentation")
hist(DataLog$BenefVert, breaks="FD", main="BenefVert", ylab="", xlab="Benef - Vertebr. Sp.")

par(mfrow=c(5, 2), mar=c(3, 3, 3, 1))	#Cleveland plots again, after some log transfos
dotchart(DataLog$Costs, main="Management Costs", groups=DataPanel$State)
dotchart(DataLog$Area, main="Area", groups=DataLog$State)
dotchart(DataLog$PP, main="Cost of Acquisition", groups=DataLog$State)
dotchart(DataLog$ChapterActivity, main="Chapter Activity", groups=DataPanel$State)
dotchart(DataLog$Elevation, main="Elevation", groups=DataLog$State)
dotchart(DataLog$RugosityMean, main="Rugosity", groups=DataLog$State)
dotchart(DataLog$DistOff, main="Distance from TNC Office", groups=DataLog$State)
dotchart(DataLog$AgriArea, main="Proprotion of\nagricultural land within 5km", groups=DataLog$State)
dotchart(DataLog$PAsEAS, main="Proprotion of\nprotected land within 5km", groups=DataLog$State)
dotchart(DataLog$Visitability, main="Visitability Index", groups=DataLog$State)


par(mfrow=c(3, 2), mar=c(3, 3, 3, 1)) # Costs repartition across factorial variables
boxplot(DataLog$Costs ~ DataLog$FireYr, xlab = "Prescribed burn that year (Y/N)", ylab = "Cost", main = "Prescribed burn years")
boxplot(DataLog$Costs ~ DataLog$IdealCond, xlab = "Ideal Condition or Not When Purchased", ylab = "Cost", main = "Ecological condition at Purchase")
boxplot(DataLog$Costs ~ DataLog$MngedFor, xlab = "Managed for particular habitat (Y/N)", ylab = "Cost", main = "Targeted habitat management")
boxplot(DataLog$Costs ~ DataLog$YearAcq, xlab = "Year", ylab = "Cost", main = "Year of Acquisition")
boxplot(DataLog$Costs ~ DataLog$F_type, xlab = "Forest Type", ylab = "Cost", main = "Forest Type")
boxplot(DataLog$Costs ~ DataLog$Invasive, xlab = "Invasive", ylab = "Cost", main = "Invasive Species")



### Checking for Collinearity
#----------------------------

CrossVar=NULL
CrossVar=cbind(Costs=DataLog$Costs, Area=DataLog$Area, Elevation=DataLog$Elevation, Rugosity=DataLog$RugosityMean, PurchPrice=DataLog$PP, DistOff=DataLog$DistOff, AgriArea=DataLog$AgriArea, PAsEAS=DataLog$PAsEAS, Visitability=DataLog$Visitability, ChapterActivity=DataLog$ChapterActivity, FireYear=DataLog$FireYr, YrSinceProt=DataLog$YrSinceProt, YearAcq=DataLog$YearAcq, Managed=DataLog$MngedFor, IdealCond=DataLog$IdealCond, F_type=DataLog$F_type)

# Pairwise Plots
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  pround <- round(test$p.value,3)
  Signif <- ifelse(pround<0.001,"p<0.001",paste("p=",pround))
  color <- ifelse(pround<0.005, ifelse(pround<0.001, "red", "orange"), "black")
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif, col=color)
}
panel.smooth<-function (x, y, col="black", bg=NA, pch=18, cex=1, col.smooth="red", span=2/3, iter=3, ...) 
{
  points(x, y, pch=pch, col=col, bg=bg, cex=cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
}
pairs(CrossVar, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, cex.labels=1)
# nothing bigger than 0.45, except one at 0.6... let's check VIFs

# VIFs
library(car)
vif(lm(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+YearAcq+MngedFor+IdealCond+F_type, data=DataLog))
# GVIF^(1/(2*Df)) < sqt(5) = 2.24 - only potential problem is AgriArea at 2.37, close enough, let's proceed an check residuals



#===============================
# Model Building and Selection
#===============================

library(lmerTest)
library(MuMIn)


### Full model fitting
#---------------------

Model0=lm(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type, data=DataLog)
AICc(Model0)
summary(Model0)
par(mfrow=c(2,2)) #Residual checking plots with Cook Dist
CookLim = 4/(dim(DataLog)[1]-(length(Model0$coefficients)-2))
plot(Model0, cook.levels=CookLim)

par(mfrow=c(5,3)) #Residuals against variables plots
plot(DataLog$Area, resid(Model0), xlab="Area", ylab="")
abline(0,0)
plot(DataLog$Elevation, resid(Model0), xlab="Elevation", ylab="")
abline(0,0)
plot(DataLog$RugosityMean, resid(Model0), xlab="Rugosity", ylab="")
abline(0,0)
plot(DataLog$PP, resid(Model0), xlab="Purchase Price", ylab="")
abline(0,0)
plot(DataLog$DistOff, resid(Model0), xlab="Distance from Office", ylab="")
abline(0,0)
plot(DataLog$AgriArea, resid(Model0), xlab="Agricultural Area", ylab="")
abline(0,0)
plot(DataLog$PAsEAS, resid(Model0), xlab="Protected Area", ylab="")
abline(0,0)
plot(DataLog$Visitability, resid(Model0), xlab="Visitability", ylab="")
abline(0,0)
plot(DataLog$ChapterActivity, resid(Model0), xlab="Chapter Activity", ylab="")
abline(0,0)
plot(DataLog$FireYr, resid(Model0), xlab="Prescribed Burn", ylab="")
abline(0,0)
plot(DataLog$YrSinceProt, resid(Model0), xlab="Time Since Protection", ylab="")
abline(0,0)
plot(DataLog$YearAcq, resid(Model0), xlab="Year of Acquisition", ylab="")
abline(0,0)
plot(DataLog$MngedFor, resid(Model0), xlab="Active Management", ylab="")
abline(0,0)
plot(DataLog$IdealCond, resid(Model0), xlab="Ideal at Acquisition", ylab="")
abline(0,0)
plot(DataLog$F_type, resid(Model0), xlab="Forest Type", ylab="")
abline(0,0)


## Check against variables that we could include as random: Name is a strong candidate, State maybe, Year / Yr Acq seem relatively benine
Res = rstandard(Model0)

boxplot(Res~Name, data=DataLog, axes=FALSE, ylim=c(-3.5,3))
abline(0,0); axis(2); title("Residuals ~ Name", line = -2)
text(1:length(levels(DataLog$Name)), -2.8, levels(DataLog$Name), cex=0.75, srt=65)

boxplot(Res~Year, data=DataLog, axes=FALSE, ylim=c(-3,3))
abline(0,0); axis(2); title("Residuals ~ Year", line = -3)
text(1:length(levels(as.factor(DataLog$Year))), -3, levels(as.factor(DataLog$Year)), cex=1, srt=30)

boxplot(Res~State, data=DataLog, axes=FALSE, ylim=c(-3.5,3))
abline(0,0); axis(2); title("Residuals ~ State", line = -0.8)
text(1:length(levels(DataLog$State)), -3, levels(DataLog$State), cex=1)

boxplot(Res~YearAcq, data=DataLog, axes=FALSE, ylim=c(-3.5,3))
abline(0,0); axis(2); title("Residuals ~ Year of Acquisition", line = -0.8)
text(1:length(levels(DataLog$YearAcq)), -3, levels(DataLog$YearAcq), cex=1)


### Random component selection
#-----------------------------

# There's a scaling warning but I'd rather not scale this model (no problem in the estimation power of the model)
Model1.1=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|State)+(1|Year)+(1|YearAcq), data=DataLog, REML=TRUE)
# and then all combinations of those 4 possible random variables
Model1.2=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|State)+(1|Year)+(1|YearAcq), data=DataLog, REML=TRUE)
Model1.3=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|Year)+(1|YearAcq), data=DataLog, REML=TRUE)
Model1.4=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|State)+(1|YearAcq), data=DataLog, REML=TRUE)
Model1.5=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|State)+(1|Year), data=DataLog, REML=TRUE)
Model1.6=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Year)+(1|YearAcq), data=DataLog, REML=TRUE)
Model1.7=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|State)+(1|YearAcq), data=DataLog, REML=TRUE)
Model1.8=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|State)+(1|Year), data=DataLog, REML=TRUE)
Model1.9=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|YearAcq), data=DataLog, REML=TRUE)
Model1.10=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|Year), data=DataLog, REML=TRUE)
Model1.11=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE)
Model1.12=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name), data=DataLog, REML=TRUE)
Model1.13=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|State), data=DataLog, REML=TRUE)
Model1.14=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Year), data=DataLog, REML=TRUE)
Model1.15=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|YearAcq), data=DataLog, REML=TRUE)

AICc(Model1.1)
AICc(Model1.2)
AICc(Model1.3)
AICc(Model1.4)
AICc(Model1.5)
AICc(Model1.6)
AICc(Model1.7)
AICc(Model1.8)
AICc(Model1.9)
AICc(Model1.10)
AICc(Model1.11) #Best model, AICc = 1515.883
AICc(Model1.12)
AICc(Model1.13)
AICc(Model1.14)
AICc(Model1.15)

# Check 1 - YearAcq as a non random variable
# It works pretty well, but there is really no point in estimating it everytime. I'll still check below if it changes anything for the model dredging and step-wise selections to have it or not as a non random variable, though.
Model2.1=Model1.11
Model2.2=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+YearAcq+MngedFor+IdealCond+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE)
AICc(Model2.1)
AICc(Model2.2) # Best: 1510.565

#Let's look at what this gives us
Model3=Model2.1
summary(Model3)
Model4=Model2.2
summary(Model4)

# Check 2 -  Alternative cost measures for purchase price: PP vs TotCost vs FMV...
Model2.3=lmer(Costs~Area+Elevation+RugosityMean+FMV+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+YearAcq+MngedFor+IdealCond+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE)
AICc(Model2.3) # Meh: 1533.437 and same (1533.083) with FMV so keep PP


### Fixed component "selection"
#------------------------------

step(Model0) # Costs ~ Area + Elevation + RugosityMean + PP + AgriArea + PAsEAS + Visitability + FireYr + YearAcq + MngedFor + F_type
step(Model3) # Costs ~ Area + RugosityMean + PP + Visitability + FireYr + F_type + (1 | Name) + (1 | State)
step(Model4) # Costs ~ Area + RugosityMean + PP + Visitability + FireYr + F_type + (1 | Name) + (1 | State)

Model3=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE, na.action = "na.fail")
r.squaredGLMM(Model3)
AllModels3 = dredge(Model3, REML=FALSE, extra="R^2") #ranking by AICc using ML, but presents the results with their REML estimators (see get.models)
head(AllModels3, 30)

Besties3 = head(AllModels3, 5) #the 5 first models that are within DeltaAIC <2
get.models(Besties3, subset=TRUE, method="REML") #just checking that estimates are given with REML even if the ranking of model is specified with REML=FALSE
coefTable(Besties3) #to get the standard errors

AvgModel3 = model.avg(Besties3)
summary(AvgModel3) # Full average = average over all set (some models will have some var at 0), conditional average = averaged over non-0 only
r.squaredLR(AvgModel3) # returns: "do not know (yet) how to construct a null model for class ‘averaging’" - see below for R-GLMM alternative

# Just to check ....
# 1) if it's robust to YearAcq => it is, we get EXACTLY the same thing
Model4=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+YearAcq+MngedFor+IdealCond+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE, na.action = "na.fail")
AllModels4 = dredge(Model4, REML=FALSE, extra="R^2")
Besties4 = head(AllModels4, 5)
AvgModel4 = model.avg (Besties4)
summary(AvgModel4)
r.squaredLR(AvgModel4)
#2) and  with TotCost? => it becomes less informative (r² are smaller, AIC was higher too, see above, etc.), let's no go that route
Model5=lmer(Costs~Area+Elevation+RugosityMean+TotCost+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+YearAcq+MngedFor+IdealCond+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE, na.action = "na.fail")
AllModels5 = dredge(Model5, REML=FALSE, extra="R^2")
head(AllModels5, 30)
Besties5 = head(AllModels5, 19)
AvgModel5 = model.avg (Besties5)
summary(AvgModel5)
r.squaredLR(AvgModel5)


### Checking  our "averaged" model
#---------------------------------
# NB: this is not the averaged model, this is the linear model with the structure of the average model (what was significant only)
AvgModel=lmer(Costs~Area+RugosityMean+PP+Visitability+FireYr+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE)
summary(AvgModel)

par(mfrow=c(1,2)) # Residual's normality
plot(fitted(AvgModel)~resid(AvgModel), xlab="Residuals", ylab="Fitted", main="Fitted values against Residuals", cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
fit.mod = lm(fitted(AvgModel)~resid(AvgModel))
abline(fit.mod)
m<-mean(resid(AvgModel))
std<-sqrt(var(resid(AvgModel)))
hist(resid(AvgModel), density=10, prob=TRUE, xlab="Residuals", main="Residuals Distribution", cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE, yaxt="n")

par(mfrow=c(5,3), mar=c(2,2,2,2)) #Residuals against variables plots
plot(DataLog$Area, residuals(AvgModel), main="Area", xlab="")
abline(0,0)
plot(DataLog$Elevation, residuals(AvgModel), main="Elevation", xlab="")
abline(0,0)
plot(DataLog$RugosityMean, residuals(AvgModel), main="Rugosity", xlab="")
abline(0,0)
plot(DataLog$PP, residuals(AvgModel), main="Acquisition Cost", xlab="")
abline(0,0)
plot(DataLog$DistOff, residuals(AvgModel), main="Distance from Office", xlab="")
abline(0,0)
plot(DataLog$AgriArea, residuals(AvgModel), main="Agricultural Area", xlab="")
abline(0,0)
plot(DataLog$PAsEAS, residuals(AvgModel), main="Protected Area", xlab="")
abline(0,0)
plot(DataLog$Visitability, residuals(AvgModel), main="Visitability", xlab="")
abline(0,0)
plot(DataLog$ChapterActivity, residuals(AvgModel), main="Chapter Activity", xlab="")
abline(0,0)
plot(DataLog$FireYr, residuals(AvgModel), main="Prescribed Burn", xlab="")
abline(0,0)
plot(DataLog$YrSinceProt, residuals(AvgModel), main="Time Since Protection", xlab="")
abline(0,0)
plot(DataLog$YearAcq, residuals(AvgModel), main="Year of Acquisition", xlab="")
abline(0,0)
plot(DataLog$MngedFor, residuals(AvgModel), main="Active Management", xlab="")
abline(0,0)
plot(DataLog$IdealCond, residuals(AvgModel), main="Ideal at Acquisition", xlab="")
abline(0,0)
plot(DataLog$F_type, residuals(AvgModel), main="Forest Type", xlab="")
abline(0,0)

par(mfrow=c(3,1), mar=c(2,2,2,2))  #Residuals against random variables plots
boxplot(residuals(AvgModel)~Name, data=DataLog, axes=FALSE, ylim=c(-5,3))
abline(0,0); axis(2); title("Residuals ~ Name", line=0)
text(1:length(levels(DataLog$Name)), -3.5, levels(DataLog$Name), cex=0.75, srt=65)

boxplot(residuals(AvgModel)~State, data=DataLog, axes=FALSE, ylim=c(-3,3.5))
abline(0,0); axis(2); title("Residuals ~ State", line=0)
text(1:length(levels(DataLog$State)), -3, levels(DataLog$State), cex=1)

boxplot(residuals(AvgModel)~Year, data=DataLog, axes=FALSE, ylim=c(-3,3.5))
abline(0,0); axis(2); title("Residuals ~ Year", line=0)
text(1:length(levels(DataLog$State)), -3, levels(DataLog$State), cex=1)



### Looking at relationship with Benefits
#----------------------------------------

par(mfrow=c(3,1))
### REPORTED BENEFIT
summary(aov(Costs~Benef, data=DataLog))
TukeyHSD(aov(Costs~Benef, data=DataLog))
boxplot(Costs~Benef, data=DataLog, notch=TRUE,  main="(a) Perceived Benefits")
text(x=c(12.3,12.3,12.3),c("*", "**", "***"),cex=2)
### BENEF - FRANGMENTATION
fit1 = glm(log(DataNoTime$AvgCostPerKm2)~log(DataNoTime$BenefFrag))
fitsum1 = summary(fit1)
plot(log(DataNoTime$BenefFrag), log(DataNoTime$AvgCostPerKm2), xlab="", ylab="Costs", main = "(b) Change in Effective Mesh Size")
abline(fit1, col="red")
r1 = fitsum1$coefficients[2,1]
myP1 = fitsum1$coefficients[2,4]
rp = vector('expression',1)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
		list(MYVALUE = format(r1,digits=2)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
		list(MYOTHERVALUE = format(myP1, digits=2)))[2]
legend('topright', legend = rp, bty = 'n')
### BENEF - VERTEBRATES
fit2 = glm(log(DataNoTime$AvgCostPerKm2)~log(DataNoTime$BenefVert))
fitsum2 = summary(fit2)
plot(log(DataNoTime$BenefVert), log(DataNoTime$AvgCostPerKm2), xlab="", ylab="Costs", main = "(c) Vertebrate Richness")
abline(fit2, col="red")
r2 = fitsum2$coefficients[2,1]
myP2 = fitsum2$coefficients[2,4] # use "< 0.001" if applicable
rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
		list(MYVALUE = format(r2,digits=2)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
		list(MYOTHERVALUE = format(myP2, digits=2)))[2]
legend('bottomright', legend = rp, bty = 'n')
### BENEF - VERTEBRATES and PP
fit = lm(log(DataNoTime$PP+1)~DataNoTime$BenefVert)
fitsum = summary(fit)
plot(log(DataNoTime$PP+1), DataNoTime$BenefVert, xlab="BenefVert", ylab="Costs", main = "Average Management Costs and overlap with Vertebrate metrics\nArmsworth et al. Biocons 2018")
abline(fit, col="red")
r2 = fitsum$coefficients[2,1]
myP = fitsum$coefficients[2,4]

rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE),
	list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE),
	list(MYOTHERVALUE = format(myP, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')


### Stats on Endowments
#----------------------

Endow = DataNoTime[,c("PBRS", "AvgCostPerKm2", "FMV", "Area")]
Endow$Invest = Endow$Area*23300 # x 233 for the endowment value, but also *100 to make it per Ha
length(which(Endow$Invest>Endow$FMV))
Endow$EndowPercent = (Endow$Invest/Endow$FMV)*100
mean(Endow$EndowPercent)


### Moran test on Avg cost/PA
#----------------------------
library(ape)
setwd('C:/Users/Diane_LeBouille/Documents/01_Thesis Work/2019_Appalachians/Model')
MoranData = read.table(file='MoranTest.txt', header=TRUE, sep="\t", na.strings='NA')
Geog = cbind(MoranData$Long, MoranData$Lat)
DistMatrix = as.matrix(dist(Geog))
DistMatrix.Inv = 1/DistMatrix
diag(DistMatrix.Inv) = 0

Moran.I(MoranData$AvgMngmt, DistMatrix.Inv, alternative="two.sided") # Nope, we good


### Temporal autocorre test
#--------------------------
PerSiteMoran = NULL
for (i in unique(DataPanel$Name)){
	Data = DataPanel[which(DataPanel$Name==i),]
	GeogTemp = cbind(Data$Year, 1)
	DistMatrix = as.matrix(dist(GeogTemp))
	DistMatrix.Inv = 1/DistMatrix
	diag(DistMatrix.Inv) = 0
	MoranTest = Moran.I(Data$Costs, DistMatrix.Inv, alternative="two.sided")
	PerSiteMoran = c(PerSiteMoran, MoranTest$p.value)
}
PerSiteMoran
which(PerSiteMoran<=0.05)
mean(PerSiteMoran)


### Additional Checks for Reviewers
#----------------------------------
CheckModel1 = lm(Costs~ChapterActivity, data=DataLog)
summary(CheckModel1)

summary(aov(ChapterActivity~Benef, data=DataLog))
TukeyHSD(aov(ChapterActivity~Benef, data=DataLog))
boxplot(ChapterActivity~Benef, data=DataLog, notch=TRUE,  main="Chapter Budget ~ Perceived Benefits")
text(x=c(16,16,16),c("", "", "*"),cex=2)


### Better R² Calculations
#-------------------------
# NB on R² in mixed effect models:
# Marginal R2 provides the variance explained only by fixed effects
# Conditional R2 provides the variance explained by the entire model, i.e. both fixed effects and random effects
Model3=lmer(Costs~Area+Elevation+RugosityMean+PP+DistOff+AgriArea+PAsEAS+Visitability+ChapterActivity+FireYr+YrSinceProt+MngedFor+IdealCond+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE)
r.squaredGLMM(Model3)

AvgModel=lmer(Costs~Area+RugosityMean+PP+Visitability+FireYr+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE)
r.squaredGLMM(AvgModel)
Best1=lmer(Costs~Area+RugosityMean+PP+Visitability+FireYr+F_type+(1|Name)+(1|State), data=DataLog, REML=TRUE)
Best2=lmer(Costs~Area+RugosityMean+PP+Visitability+FireYr+F_type+AgriArea+(1|Name)+(1|State), data=DataLog, REML=TRUE)
Best3=lmer(Costs~Area+RugosityMean+PP+Visitability+FireYr+F_type+YrSinceProt+(1|Name)+(1|State), data=DataLog, REML=TRUE)
Best4=lmer(Costs~Area+RugosityMean+PP+Visitability+FireYr+F_type+ChapterActivity+(1|Name)+(1|State), data=DataLog, REML=TRUE)
Best5=lmer(Costs~Area+RugosityMean+PP+Visitability+FireYr+F_type+DistOff+(1|Name)+(1|State), data=DataLog, REML=TRUE)
r.squaredGLMM(Best1)
r.squaredGLMM(Best2)
r.squaredGLMM(Best3)
r.squaredGLMM(Best4)
r.squaredGLMM(Best5)
