library(ggplot2)
library(dplyr)
library(descr)

##################Filtering the Data set to Only Include EWBW Eligible People###################################
EWBW <- read.csv("~/Downloads/EWBWEvaluation_ALL_DATA_Clean_March_2025.csv")
#Subset to people who said yes: freq(EWBW$Do.you.participate.in.the.Supplemental.Nutrition.Assistance.Program..SNAP...SNAP.is.sometimes.known.an.EBT.card.or.food.stamps.)
names(EWBW)[names(EWBW)== "Do.you.participate.in.the.Supplemental.Nutrition.Assistance.Program..SNAP...SNAP.is.sometimes.known.an.EBT.card.or.food.stamps."] <- "Eligible"
#Number of Years on SNAP
names(EWBW)[names(EWBW)== "How.long.have.you.been.receiving.SNAP.benefits...also.known.as.food.stamps..or.EBT...."] <- "Years_on_SNAP"
EWBW$Years_on_SNAP[EWBW$Years_on_SNAP==""] <- "NA"
EWBW$Years_on_SNAP[EWBW$Years_on_SNAP=="Choose not to answer"] <- "NA"
EWBW$Years_on_SNAP[EWBW$Years_on_SNAP=="Unsure"] <- "NA"
EWBW$Years_on_SNAP[EWBW$Years_on_SNAP=="NA"] <- NA

#If SNAP Adequately supports the household per month
names(EWBW)[names(EWBW)== "Do.you.feel.that.your.SNAP.benefits.give.you.enough.money.to.feed.your.household.each.month."] <- "Adequate"
EWBW$Adequate[EWBW$Adequate==""] <- "NA"
EWBW$Adequate[EWBW$Adequate=="Choose not to answer"] <- "NA"
EWBW$Adequate[EWBW$Adequate=="Unsure"] <- "NA"
EWBW$Adequate[EWBW$Adequate=="NA"] <- NA

#Number of Weeks SNAP would last per month
names(EWBW)[names(EWBW)== "How.many.weeks.do.your.SNAP.benefits.typically.last.during.an.average.month."] <- "Weekspermonth_on_SNAP"
EWBW$Weekspermonth_on_SNAP[EWBW$Weekspermonth_on_SNAP==""] <- "NA"
EWBW$Weekspermonth_on_SNAP[EWBW$Weekspermonth_on_SNAP=="Choose not to answer"] <- "NA"
EWBW$Weekspermonth_on_SNAP[EWBW$Weekspermonth_on_SNAP=="Unsure"] <- "NA"
EWBW$Weekspermonth_on_SNAP[EWBW$Weekspermonth_on_SNAP=="NA"] <- NA

#Awareness of EWBW Variables - Consolidation
names(EWBW)[names(EWBW)== "Have.you.noticed.that.you.ve.been.receiving.money.back.on.your.EBT.card.when.purchasing.fresh.fruits.and.vegetables."] <- "MoneyBack"
names(EWBW)[names(EWBW)== "Had.you.heard.of.the.Eat.Well..Be.Well.program.before.receiving.the.letter.that.invited.you.to.take.this.survey..."] <- "HEARDABOUT_EWBW"
EWBW$MoneyBack[EWBW$MoneyBack==""] <- "NA"
EWBW$MoneyBack[EWBW$MoneyBack=="Choose not to answer"] <- "NA"
EWBW$HEARDABOUT_EWBW[EWBW$HEARDABOUT_EWBW==""] <- "NA"
EWBW$HEARDABOUT_EWBW[EWBW$HEARDABOUT_EWBW=="Choose not to answer"] <- "NA"

EWBW$Awareness[EWBW$MoneyBack== "NA"& EWBW$HEARDABOUT_EWBW=="NA"]<-"NA"
EWBW$Awareness[EWBW$MoneyBack== "Unsure"| EWBW$HEARDABOUT_EWBW=="Unsure"]<-"NA"
EWBW$Awareness[EWBW$MoneyBack== "No"| EWBW$HEARDABOUT_EWBW=="No"]<-"No"
EWBW$Awareness[EWBW$MoneyBack== "Yes"| EWBW$HEARDABOUT_EWBW=="Yes"]<-"Yes"
EWBW$HEARDABOUT_EWBW[EWBW$HEARDABOUT_EWBW=="NA"] <- NA

##Creating a variable to highlight if an individual is eligible to participate or not
EWBW <- EWBW %>%
  mutate(
    not_eligible = if_else(
      is.na(Years_on_SNAP ) &
        is.na(Adequate) &
        is.na(Weekspermonth_on_SNAP) &
        is.na(HEARDABOUT_EWBW),
      1, 0
    )
  )
freq(EWBW$not_eligible)

# Exclude those not eligible to participate (n=1216)
EWBW <- EWBW %>% filter(not_eligible == 0)

##Age + Race Cleaning Up############################################################################################################

#Age
names(EWBW)[names(EWBW)== "How.old.are.you."] <- "Age"
EWBW$Age[EWBW$Age>120]<- NA
EWBW$Age<-as.numeric(EWBW$Age)


#Race Variables (Hispanic vs Not, Black vs Not, White vs Not)
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.African.American.or.Black."] <- "African American/Black"
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.Caucasian.or.White."] <- "Caucasian/White"
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.American.Indian.or.Alaskan.Native."] <- "American Indian/Alaskan Native"
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.Native.Hawaiian.or.Other.Pacific.Islander."] <- "Native Hawaiian/Pacific Islander"
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.Asian."] <- "Asian"
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.Other..please.describe..."] <- "Other_Describe"
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.Choose.not.to.answer."] <- "Not_To_Answer"
names(EWBW)[names(EWBW)== "Which.of.the.following.best.describes.your.race..Please.select.all.that.apply..choice.Dont.Know."] <- "Don't_Know"
names(EWBW)[names(EWBW)== "Other..please.describe."] <- "Other_Description"

################################################################################################
#Race
EWBW$race<-NA
EWBW$race[EWBW$`African American/Black`=="Checked"]<-"African American/Black"
EWBW$race[EWBW$`Caucasian/White`=="Checked"]<-"Caucasian/White"
EWBW$race[EWBW$`American Indian/Alaskan Native`=="Checked"]<-"American Indian/Alaskan Native"
EWBW$race[EWBW$`Native Hawaiian/Pacific Islander`=="Checked"]<-"Native Hawaiian/Pacific Islander"
EWBW$race[EWBW$`Asian`=="Checked"]<-"Asian"
EWBW$race[EWBW$`Other_Describe`=="Checked"]<-"Other"

#Mixed-Race
EWBW$race[(EWBW$`Asian`=="Checked" & EWBW$`Native Hawaiian/Pacific Islander`=="Checked") | (EWBW$`Asian`=="Checked" & EWBW$`American Indian/Alaskan Native`=="Checked")]<-"Mixed"
EWBW$race[(EWBW$`Asian`=="Checked" & EWBW$`Caucasian/White`=="Checked") | (EWBW$`Asian`=="Checked" & EWBW$`African American/Black`=="Checked")]<-"Mixed"
EWBW$race[(EWBW$`African American/Black`=="Checked"&EWBW$`Caucasian/White`=="Checked") | (EWBW$`African American/Black`=="Checked"&EWBW$`American Indian/Alaskan Native`=="Checked") | (EWBW$`African American/Black`=="Checked"&EWBW$`Native Hawaiian/Pacific Islander`=="Checked")]<-"Mixed"
EWBW$race[(EWBW$`Caucasian/White`=="Checked" & EWBW$`American Indian/Alaskan Native`=="Checked") | (EWBW$`Caucasian/White`=="Checked" & EWBW$`Native Hawaiian/Pacific Islander`=="Checked")]<-"Mixed"
EWBW$race[(EWBW$`American Indian/Alaskan Native`=="Checked" & EWBW$`Native Hawaiian/Pacific Islander`=="Checked")]<-"Mixed"

#Code as NA
EWBW$race[EWBW$`Not_To_Answer`=="Checked"]<-"Not_To_Answer"
EWBW$race[EWBW$`Don't_Know`=="Checked"]<-"Don't_Know"
EWBW$race[EWBW$race=="Not_To_Answer" | EWBW$race=="Don't_Know"]<-"NA"

#White Variable
EWBW$White[EWBW$race!="Caucasian/White"]<-0
EWBW$White[EWBW$race=="Caucasian/White"]<-1

#Black Variable
EWBW$Black[EWBW$race!="African American/Black"]<-0
EWBW$Black[EWBW$race=="African American/Black"]<-1

#Ethnicity Variables
names(EWBW)[names(EWBW)== "Are.you.Hispanic.or.Latino.a.in.origin.or.descent."] <- "Hispanic_Latino"
EWBW$Hispanic_Latino[EWBW$Hispanic_Latino==""] <- "NA"
EWBW$Hispanic_Latino[EWBW$Hispanic_Latino=="Choose not to answer"] <- "NA"
EWBW$Hispanic_Latino[EWBW$Hispanic_Latino=="Don't Know"] <- "NA"

#Household Information
names(EWBW)[names(EWBW)== "What.is.your.annual.household.income..before.taxes.."] <- "Household_Income1"
names(EWBW)[names(EWBW)== "What.is.your.annual.household.income..before.taxes...1"] <- "Household_Income2"
names(EWBW)[names(EWBW)== "What.is.your.annual.household.income..before.taxes...2"] <- "Household_Income3"
names(EWBW)[names(EWBW)== "What.is.your.annual.household.income..before.taxes...3"] <- "Household_Income4"
names(EWBW)[names(EWBW)== "What.is.your.annual.household.income..before.taxes...4"] <- "Household_Income5"
names(EWBW)[names(EWBW)== "What.is.your.annual.household.income..before.taxes...5"] <- "Household_Income6"
names(EWBW)[names(EWBW)== "What.is.your.annual.household.income..before.taxes...6"] <- "Household_Income7"

EWBW$Household_Income1[EWBW$Household_Income1==""] <- "NA"
EWBW$Household_Income1[EWBW$Household_Income1=="Choose not to answer"] <- "NA"
EWBW$Household_Income1[EWBW$Household_Income1=="Don't know"] <- "NA"

EWBW$Household_Income2[EWBW$Household_Income2==""] <- "NA"
EWBW$Household_Income2[EWBW$Household_Income2=="Choose not to answer"] <- "NA"
EWBW$Household_Income2[EWBW$Household_Income2=="Don't know"] <- "NA"

EWBW$Household_Income3[EWBW$Household_Income3==""] <- "NA"
EWBW$Household_Income3[EWBW$Household_Income3=="Choose not to answer"] <- "NA"
EWBW$Household_Income3[EWBW$Household_Income3=="Don't know"] <- "NA"

EWBW$Household_Income4[EWBW$Household_Income4==""] <- "NA"
EWBW$Household_Income4[EWBW$Household_Income4=="Choose not to answer"] <- "NA"
EWBW$Household_Income4[EWBW$Household_Income4=="Don't know"] <- "NA"

EWBW$Household_Income5[EWBW$Household_Income5==""] <- "NA"
EWBW$Household_Income5[EWBW$Household_Income5=="Choose not to answer"] <- "NA"
EWBW$Household_Income5[EWBW$Household_Income5=="Don't know"] <- "NA"

EWBW$Household_Income6[EWBW$Household_Income6==""] <- "NA"
EWBW$Household_Income6[EWBW$Household_Income6=="Choose not to answer"] <- "NA"
EWBW$Household_Income6[EWBW$Household_Income6=="Don't know"] <- "NA"

EWBW$Household_Income7[EWBW$Household_Income7==""] <- "NA"
EWBW$Household_Income7[EWBW$Household_Income7=="Choose not to answer"] <- "NA"
EWBW$Household_Income7[EWBW$Household_Income7=="Don't know"] <- "NA"

#EWBW$Real_Income[EWBW$Real_Income=="Choose not to answer"] <- "NA"
#EWBW$Real_Income[EWBW$Real_Income=="Don't know"] <- "NA"

EWBW$Household_Income1[EWBW$Household_Income1=="NA"] <- NA
EWBW$Household_Income2[EWBW$Household_Income2=="NA"] <- NA
EWBW$Household_Income3[EWBW$Household_Income3=="NA"] <- NA
EWBW$Household_Income4[EWBW$Household_Income4=="NA"] <- NA
EWBW$Household_Income5[EWBW$Household_Income5=="NA"] <- NA
EWBW$Household_Income6[EWBW$Household_Income6=="NA"] <- NA
EWBW$Household_Income7[EWBW$Household_Income7=="NA"] <- NA

EWBW <- EWBW %>%
  mutate(
    poverty_level = case_when(
      # Household size = 1
      !is.na(Household_Income1) & Household_Income1 == "Below $15,000"                ~ "<100%",
      !is.na(Household_Income1) & Household_Income1 == "$15,000 to $30,000"           ~ "100%-200%",
      !is.na(Household_Income1) & Household_Income1 == "$30,001 to $45,000"           ~ "200%-300%",
      !is.na(Household_Income1) & Household_Income1 == "$45,001 to $60,000"           ~ "300%-400%",
      !is.na(Household_Income1) & Household_Income1 == "$60,001 to $75,000"           ~ "400%-500%",
      !is.na(Household_Income1) & Household_Income1 == "Above $75,000"                ~ ">500%",

      # Household size = 2
      !is.na(Household_Income2) & Household_Income2 == "Below $20,000"                ~ "<100%",
      !is.na(Household_Income2) & Household_Income2 == "$20,001 to $40,000"           ~ "100%-200%",
      !is.na(Household_Income2) & Household_Income2 == "$40,001 to $60,000"           ~ "200%-300%",
      !is.na(Household_Income2) & Household_Income2 == "$60,001 to $80,000"           ~ "300%-400%",
      !is.na(Household_Income2) & Household_Income2 == "$80,001 to $100,000"          ~ "400%-500%",
      !is.na(Household_Income2) & Household_Income2 == "Above $100,000"               ~ ">500%",

      # Household size = 3
      !is.na(Household_Income3) & Household_Income3 == "Below $25,000"                ~ "<100%",
      !is.na(Household_Income3) & Household_Income3 == "$25,001 to $50,000"           ~ "100%-200%",
      !is.na(Household_Income3) & Household_Income3 == "$50,001 to $75,000"           ~ "200%-300%",
      !is.na(Household_Income3) & Household_Income3 == "$75,001 to $100,000"          ~ "300%-400%",
      !is.na(Household_Income3) & Household_Income3 == "$100,001 to $125,000"         ~ "400%-500%",
      !is.na(Household_Income3) & Household_Income3 == "Above $125,000"               ~ ">500%",

      # Household size = 4
      !is.na(Household_Income4) & Household_Income4 == "Below $30,000"                ~ "<100%",
      !is.na(Household_Income4) & Household_Income4 == "$30,001 to $60,000"           ~ "100%-200%",
      !is.na(Household_Income4) & Household_Income4 == "$60,001 to $90,000"           ~ "200%-300%",
      !is.na(Household_Income4) & Household_Income4 == "$90,001 to $120,000"          ~ "300%-400%",
      !is.na(Household_Income4) & Household_Income4 == "$120,001 to $150,000"         ~ "400%-500%",
      !is.na(Household_Income4) & Household_Income4 == "Above $150,000"               ~ ">500%",

      # Household size = 5
      !is.na(Household_Income5) & Household_Income5 == "Below $35,000"                ~ "<100%",
      !is.na(Household_Income5) & Household_Income5 == "$35,001 to $70,000"           ~ "100%-200%",
      !is.na(Household_Income5) & Household_Income5 == "$70,001 to $105,000"          ~ "200%-300%",
      !is.na(Household_Income5) & Household_Income5 == "$105,001 to $140,000"         ~ "300%-400%",
      !is.na(Household_Income5) & Household_Income5 == "$140,001 to $175,000"         ~ "400%-500%",
      !is.na(Household_Income5) & Household_Income5 == "Above $175,000"               ~ ">500%",

      # Household size = 6
      !is.na(Household_Income6) & Household_Income6 == "Below $40,000"                ~ "<100%",
      !is.na(Household_Income6) & Household_Income6 == "$40,001 to $80,000"           ~ "100%-200%",
      !is.na(Household_Income6) & Household_Income6 == "$80,001 to $120,000"          ~ "200%-300%",
      !is.na(Household_Income6) & Household_Income6 == "$120,001 to $160,000"         ~ "300%-400%",
      !is.na(Household_Income6) & Household_Income6 == "$160,001 to $200,000"         ~ "400%-500%",
      !is.na(Household_Income6) & Household_Income6 == "Above $200,000"               ~ ">500%",
      
      # Household size = 7
      !is.na(Household_Income7) & Household_Income7 == "Below $45,000"                ~ "<100%",
      !is.na(Household_Income7) & Household_Income7 == "$45,001 to $90,000"           ~ "100%-200%",
      !is.na(Household_Income7) & Household_Income7 == "$90,001 to $135,000"          ~ "200%-300%",
      !is.na(Household_Income7) & Household_Income7 == "$135,001 to $180,000"         ~ "300%-400%",
      !is.na(Household_Income7) & Household_Income7 == "$180,001 to $225,000"         ~ "400%-500%",
      !is.na(Household_Income7) & Household_Income7 == "Above $225,000"               ~ ">500%",
    )
  )
freq(EWBW$poverty_level)

# collapse poverty levels to smaller categories: <100%, 100-200%, >200%
EWBW$poverty_level_Real <- EWBW$poverty_level
EWBW$poverty_level_Real[EWBW$poverty_level %in% c("200%-300%","300%-400%","400%-500%",">500%")] <- ">200%"
freq(EWBW$poverty_level_Real)

names(EWBW)[names(EWBW)== "How.many.children.are.in.your.household."] <- "Children"
EWBW$Children<-as.numeric(EWBW$Children)
EWBW$Children[EWBW$Household.size==1] <- 0

#Turn Children into categorical: Zero, One, Two, More than Two
EWBW$ChildrenCat[EWBW$Children==0] <- "Zero"
EWBW$ChildrenCat[EWBW$Children==1] <- "One"
EWBW$ChildrenCat[EWBW$Children==2] <- "Two"
EWBW$ChildrenCat[EWBW$Children>2] <- "More than Two"

#Creating the US Household Food Security: To Identify if poverty is very low, low, or high
names(EWBW)[names(EWBW)== "X.The.food.that.we.bought.just.didn.t.last.and.we.didn.t.have.money.to.get.more...Was.that.often..sometimes..or.never.true.for..you.or.your.household.n.the.last.12.months.."] <- "HH3"
EWBW$HH3[EWBW$HH3==""] <- "NA"
EWBW$HH3[EWBW$HH3=="Choose not to answer"] <- "NA"
EWBW$HH3[EWBW$HH3=="Don't Know"] <- "NA"

names(EWBW)[names(EWBW)== "X.We.couldn.t.afford.to.eat.balanced.meals...Was.that.often..sometimes..or.never.true.for.you.or.your.household.in.the.last.12.months.."] <- "HH4"
EWBW$HH4[EWBW$HH4==""] <- "NA"
EWBW$HH4[EWBW$HH4=="Choose not to answer"] <- "NA"
EWBW$HH4[EWBW$HH4=="Don't Know"] <- "NA"

names(EWBW)[names(EWBW)== "In.the.last.12.months..since.last.April..did.you.or.other.adults.in.the.household.ever.cut.the.size.of.your.meals.or.skip.meals.because.there.wasn.t.enough.money.for.food...."] <- "AD1"
EWBW$AD1[EWBW$AD1==""] <- "NA"
EWBW$AD1[EWBW$AD1=="Choose not to answer"] <- "NA"
EWBW$AD1[EWBW$AD1=="Don't Know"] <- "NA"

names(EWBW)[names(EWBW)== "In.the.last.12.months..did.you.ever.eat.less.than.you.felt.you.should.because.there.wasn.t.enough.money.for.food......"] <- "AD2"
EWBW$AD2[EWBW$AD2==""] <- "NA"
EWBW$AD2[EWBW$AD2=="Choose not to answer"] <- "NA"
EWBW$AD2[EWBW$AD2=="Don't Know"] <- "NA"

names(EWBW)[names(EWBW)== "In.the.last.12.months..were.you.ever.hungry..but.didn.t.eat..because.there.wasn.t.enough.money.for.food......"] <- "AD3"
EWBW$AD3[EWBW$AD3==""] <- "NA"
EWBW$AD3[EWBW$AD3=="Choose not to answer"] <- "NA"
EWBW$AD3[EWBW$AD3=="Don't Know"] <- "NA"

EWBW$HH3[EWBW$HH3=="NA"] <- NA
EWBW$HH4[EWBW$HH4=="NA"] <- NA
EWBW$AD1[EWBW$AD1=="NA"] <- NA
EWBW$AD2[EWBW$AD2=="NA"] <- NA
EWBW$AD3[EWBW$AD3=="NA"] <- NA

EWBW$HH3Point <- NA
EWBW$HH3Point [EWBW$HH3=="Never true"] <- 0
EWBW$HH3Point [EWBW$HH3=="Often true" | EWBW$HH3=="Sometimes true"] <- 1

EWBW$HH4Point <- NA
EWBW$HH4Point [EWBW$HH4=="Never true"] <- 0
EWBW$HH4Point [EWBW$HH4=="Often true" | EWBW$HH4=="Sometimes true"] <- 1

EWBW$AD1Point <- NA
EWBW$AD1Point [EWBW$AD1=="No"] <- 0
EWBW$AD1Point [EWBW$AD1=="Yes, almost every month" | EWBW$AD1=="Yes, some months but not every month"] <- 2

EWBW$AD2Point <- NA
EWBW$AD2Point [EWBW$AD2=="No"] <- 0
EWBW$AD2Point [EWBW$AD2=="Yes"] <- 1

EWBW$AD3Point <- NA
EWBW$AD3Point [EWBW$AD3=="No"] <- 0
EWBW$AD3Point [EWBW$AD3=="Yes"] <- 1

EWBW$SumPoint <- EWBW$HH3Point + EWBW$HH4Point + EWBW$AD1Point + EWBW$AD2Point + EWBW$AD3Point 


EWBW$Household_Security [EWBW$SumPoint<=1] <- "High"
EWBW$Household_Security [EWBW$SumPoint>1 & EWBW$SumPoint<=4] <- "Low"
EWBW$Household_Security [EWBW$SumPoint>=5] <- "Very Low"

EWBW$AwarenessBin[EWBW$Awareness=="Yes"]<-1
EWBW$AwarenessBin[EWBW$Awareness=="No"]<-0

is.factor(EWBW$ChildrenCat)
EWBW$ChildrenCat <- factor(
  EWBW$ChildrenCat,
  levels = c("Zero", "One", "Two", "More than Two")
)
EWBW$ChildrenCat <- relevel(EWBW$ChildrenCat, ref = "Zero")


is.factor(EWBW$Years_on_SNAP)
EWBW$Years_on_SNAP <- factor(
  EWBW$Years_on_SNAP,
  levels = c("Less than a year", "1-2 years", "2-5 years", "Greater than 5 years")
)
EWBW$Years_on_SNAP <- relevel(EWBW$Years_on_SNAP, ref = "Less than a year")

##Subsetting to start analysis ##############################################################################################################
EWBW$Awareness[EWBW$Awareness=="NA"]<-NA
EWBW$Weekspermonth_on_SNAP[EWBW$Weekspermonth_on_SNAP=="NA"]<-NA
EWBW$Adequate[EWBW$Adequate=="NA"]<-NA
EWBW$Years_on_SNAP[EWBW$Years_on_SNAP=="NA"]<-NA
EWBW$Household_Income[EWBW$poverty_level_Real=="NA"]<-NA
EWBW$Hispanic_Latino[EWBW$Hispanic_Latino=="NA"]<-NA
EWBW$race[EWBW$race=="NA"]<-NA

#Descriptive statistics
summary(EWBW$Age) 
freq(EWBW$Awareness)
summary(EWBW$Children) 
freq(EWBW$Weekspermonth_on_SNAP)
freq(EWBW$Years_on_SNAP)
freq(EWBW$poverty_level_Real)
freq(EWBW$Hispanic_Latino)
freq(EWBW$race)
freq(EWBW$Household_Security)
freq(EWBW$ChildrenCat)

############Bivariate Analysis###############################################################################################################
df <- EWBW %>%
  filter(!is.na(ChildrenCat), !is.na(poverty_level_Real), !is.na(Hispanic_Latino), !is.na(Years_on_SNAP))

ggplot(data=df) +
  stat_summary(aes(x=ChildrenCat, y=AwarenessBin),  fun="mean", geom="bar", fill = "4A4A4A") + 
  ylab("Awareness of the EWBW Program") +
  xlab("Number of Children")+
  ggtitle("The number of Children is related to the Awareness of the Program")

ggplot(data=df) +
  stat_summary(aes(x=Hispanic_Latino, y=AwarenessBin),  fun="mean", geom="bar", fill = "4A4A4A") + 
  ylab("Awareness of the EWBW Program") +
  ggtitle("Hispanic/Latino Identifying People are Less Aware of the Program")

ggplot(data=df) +
  stat_summary(aes(x=Years_on_SNAP, y=AwarenessBin),  fun="mean", geom="bar", fill = "4A4A4A") + 
  ylab("Awareness of the EWBW Program") +
  ggtitle("New participants in the SNAP Program are less aware of the EWBW Program")

ggplot(data=df) +
  stat_summary(aes(x=ChildrenCat, y=AwarenessBin),  fun="mean", geom="bar", fill = "4A4A4A") + 
  facet_grid(. ~ Hispanic_Latino)+
  ylab("Awareness of the EWBW Program") +
  xlab("Number of Children")+
  ggtitle("The number of Children is related to the Awareness of the Program by Ethnicity")

ggplot(data=df) +
  stat_summary(aes(x=ChildrenCat, y=AwarenessBin),  fun="mean", geom="bar", fill = "4A4A4A") + 
  facet_grid(. ~ Years_on_SNAP)+
  ylab("Awareness of the EWBW Program") +
  xlab("Number of Children")+
  ggtitle("The number of Children is related to the Awareness of the Program by the Years on SNAP")

##########Awareness and Household Income#####################################################################################################
tab1 <- table(EWBW$Awareness, EWBW$Years_on_SNAP)
tab2 <- table(EWBW$Awareness, EWBW$Household_Security)
tab3 <- table(EWBW$Awareness, EWBW$poverty_level_Real)

##########Hypothesis Testing##################################################################################################################
myChi <- chisq.test(EWBW$Awareness, EWBW$poverty_level_Real) 
myChi 

myChi2 <- chisq.test(EWBW$Awareness, EWBW$Children) 
myChi2

myChi3 <- chisq.test(EWBW$Awareness, EWBW$Weekspermonth_on_SNAP) 
myChi3

myChi4 <- chisq.test(EWBW$Awareness, EWBW$Years_on_SNAP) 
myChi4

myChi5 <- chisq.test(EWBW$Awareness, EWBW$Hispanic_Latino) 
myChi5

myChi6 <- chisq.test(EWBW$Awareness, EWBW$White) 
myChi6

myChi7 <- chisq.test(EWBW$Awareness, EWBW$Black) 
myChi7

myChi8 <- chisq.test(EWBW$Awareness, EWBW$Mixed) 
myChi8

myChi9 <- chisq.test(EWBW$Awareness, EWBW$Household_Security) 
myChi9

myChi10 <- chisq.test(EWBW$Awareness, EWBW$ChildrenCat) 
myChi10

myAnovaResults <- aov(Age ~ Awareness, data = EWBW) 
summary(myAnovaResults)



#Logistic Regression

library(sjPlot)
my.logreg <- glm(AwarenessBin ~ ChildrenCat, data = EWBW, family = "binomial") 
summary(my.logreg)
exp(my.logreg$coefficients) 
tab_model(my.logreg)

my.logreg1 <- glm(AwarenessBin ~ ChildrenCat + poverty_level_Real, data = EWBW, family = "binomial") 
summary(my.logreg1) 

my.logreg2 <- glm(AwarenessBin ~ ChildrenCat + factor(Years_on_SNAP), data = EWBW, family = "binomial") 
summary(my.logreg2)  

my.logreg3 <- glm(AwarenessBin ~ ChildrenCat + Hispanic_Latino, data = EWBW, family = "binomial") 
summary(my.logreg3) 
exp(my.logreg3$coefficients) 

my.logreg4 <- glm(AwarenessBin ~ ChildrenCat + White + Black, data = EWBW, family = "binomial") 
summary(my.logreg4)  

my.logreg5 <- glm(AwarenessBin ~ ChildrenCat + poverty_level_Real + White + Black + Hispanic_Latino + factor(Years_on_SNAP), data = EWBW, family = "binomial") 
summary(my.logreg5)  
exp(my.logreg5$coefficients) 
tab_model(my.logreg5)

tab_model(my.logreg, my.logreg1, my.logreg2, my.logreg3, my.logreg4, my.logreg5, title = "Logistic Regression Results")

