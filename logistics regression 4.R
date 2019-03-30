##################### Importing dataset ##################################################
mydata=read.csv("E:\\Career\\R Analytics\\Data science Case studies\\Logistics regression case study\\Proactive Attrition Management-Logistic Regression Case Study.csv",na.strings=c("","NA"))



#################### Descriptive statistics for continous variables ##########################
mystats <- function(x) {
      nmiss<-sum(is.na(x))
      a <- x[!is.na(x)]
      m <- mean(a,is.na=TRUE)
      n <- length(a)
      s <- sd(a)
      min <- min(a)
      p1<-quantile(a,0.01)
      p5<-quantile(a,0.05)
      p10<-quantile(a,0.10)
      q1<-quantile(a,0.25)
      q2<-quantile(a,0.5)
      q3<-quantile(a,0.75)
      p90<-quantile(a,0.90)
      p95<-quantile(a,0.95)
      p99<-quantile(a,0.99)
      max <- max(a)
      iqr=IQR(x,na.rm=T)
      UC <-quantile(a,0.95) 
      LC <-quantile(a,0.05)
      outlier_flag<- max>UC | min<LC
      return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

######################## Descriptive statistics function for categorical variable####################
mystats_categorical<-function(x) # Statistics for categorical variable
{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  
}

# Deleting the unnecessary variable

mydata$CHURNDEP<-NULL
mydata$CSA<-NULL

#################################### Array of all continous variables ############################
continuos_var<-c("REVENUE","MOU",	"RECCHRGE","DIRECTAS","OVERAGE",	"ROAM",	"CHANGEM",	"CHANGER",	"DROPVCE",	
                 "BLCKVCE",	"UNANSVCE",	"CUSTCARE", "THREEWAY",	"MOUREC",	"OUTCALLS",	"INCALLS",	"PEAKVCE",
                 "OPEAKVCE",	"DROPBLK"	,"CALLWAIT",	"MONTHS",	"UNIQSUBS",	"ACTVSUBS",	"PHONES",	
                 "MODELS",	"EQPDAYS",	"AGE1","AGE2",	"REFER",	"INCOME",	"CREDITAD",	
                 "SETPRC")
# some unneccesary variables are removed retcalls retaccpt

# some unneccesary variables are not taken into account  which didnt add any information to dependent variable.

categorical_var<-c("CHURN","CHILDREN",	"CREDITA",	"CREDITAA",	"CREDITB",	"CREDITC",	"CREDITDE",
                   "CREDITGY",	"CREDITZ",	"PRIZMRUR",	"PRIZMUB",	"PRIZMTWN",	"REFURB",	"WEBCAP",	"TRUCK",
                   "RV",	"OCCPROF",	"OCCCLER",	"OCCCRFT",	"OCCSTUD",	"OCCHMKR",	"OCCRET",	"OCCSELF",
                   "OWNRENT",	"MARRYUN",	"MARRYYES",	"MAILORD",	"MAILRES",	"MAILFLAG",	"TRAVEL",	"PCOWN",
                   "CREDITCD",	"NEWCELLY",	"NEWCELLN",	"INCMISS",	"MCYCLE",	"SETPRCM")


all_var<-c("REVENUE","MOU",	"RECCHRGE","DIRECTAS","OVERAGE",	"ROAM",	"CHANGEM",	"CHANGER",	"DROPVCE",	
           "BLCKVCE",	"UNANSVCE",	"CUSTCARE", "THREEWAY",	"MOUREC",	"OUTCALLS",	"INCALLS",	"PEAKVCE",
           "OPEAKVCE",	"DROPBLK"	,"CALLWAIT",	"MONTHS",	"UNIQSUBS",	"ACTVSUBS",	"PHONES",	
           "MODELS",	"EQPDAYS",	"AGE1","AGE2",	"REFER",	"INCOME",	"CREDITAD",	
           "SETPRC",
           "CHURN","CHILDREN",	"CREDITA",	"CREDITAA",	"CREDITB",	"CREDITC",	"CREDITDE",
           "CREDITGY",	"CREDITZ",	"PRIZMRUR",	"PRIZMUB",	"PRIZMTWN",	"REFURB",	"WEBCAP",	"TRUCK",
           "RV",	"OCCPROF",	"OCCCLER",	"OCCCRFT",	"OCCSTUD",	"OCCHMKR",	"OCCRET",	"OCCSELF",
           "OWNRENT",	"MARRYUN",	"MARRYYES",	"MAILORD",	"MAILRES",	"MAILFLAG",	"TRAVEL",	"PCOWN",
           "CREDITCD",	"NEWCELLY",	"NEWCELLN",	"INCMISS",	"MCYCLE",	"SETPRCM")

mydata$INCOME[mydata$INCOME==0]<-NA  # As O represent misssing
mydata$SETPRC[mydata$SETPRC==0]<-NA  # As O represent misssing

##################################################################################################################
diag_stats_categorical<-t(data.frame(apply(mydata[,categorical_var], 2, mystats_categorical))) # for categorical variable
View(diag_stats_categorical)
write.csv(diag_stats_categorical,"E://Career//R Analytics//Data science Case studies//Logistics regression 4//cat_desc.csv") 
###############################################################################################################

diag_stats<-t(data.frame(apply(mydata[,continuos_var], 2, mystats)))     # for continous variable     
View(diag_stats)           
write.csv(diag_stats,"E://Career//R Analytics//Data science Case studies//Logistics regression 4//cont_desc.csv")            

#################################################################################################################



# By analyzing the descriptive summary of continous variables SETPRC has more than 50% missing values
# so I take a decision of deleting that variables
mydata$SETPRC<-NULL
continuos_var<-continuos_var[!continuos_var %in% 'SETPRC']
all_var<-all_var[!all_var %in% 'SETPRC']

###############################   Outlier Treatment  #########################################


# We cap the upper outlier with 95 percentile and lower outliers for 5 percentile 
M1_fun <- function(x){
  quantiles <- quantile(x, c(.05, .95 ),na.rm=TRUE )
  # Above line will calc the P5 and P95
  
  x[ x < quantiles[1] ] <- quantiles[1]  # if value < P5, then P1
  x[ x > quantiles[2] ] <- quantiles[2]  # if value > P95, then P95
  x
}

# Applying the func for Outlier Treatment
mydata[,continuos_var] <- apply(data.frame(mydata[,continuos_var]), 2, M1_fun) 



###############   Missing Value Treatment for continuos variables    ##############################

## SO continous variable which have missing values in less proportion are simply replaced by mean
miss_var<-c("REVENUE","MOU","RECCHRGE","DIRECTAS","OVERAGE","ROAM", "CHANGEM" ,"CHANGER",
            "PHONES", "MODELS","AGE1","AGE2" )

mydata[,miss_var] <- apply(data.frame(mydata[,miss_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
mydata[,"EQPDAYS"] <- apply(data.frame(mydata[,"EQPDAYS"]), 2, function(x){x <- replace(x, is.na(x), round(mean(x, na.rm=TRUE),0))})
mydata$EQPDAYS<-as.integer(mydata$EQPDAYS)



# INCOME have very high missing value around 25% so therefore there imputation is done in different manner
# using rpart package. I take consideration that it is imp variable so cant delete it.
require(rpart)


anova_mod_1 <- rpart(INCOME ~ ., data=mydata[!is.na(mydata$INCOME),all_var ], method="anova", na.action=na.omit)  
INCOME_P<-predict(anova_mod_1, mydata[is.na(mydata$INCOME), ])
mydata[is.na(mydata$INCOME),"INCOME" ]=INCOME_P



#Missing Value Treatment for categorical variables- replacing the missing value with mode in case of categorical variable
mydata[categorical_var] <- apply(data.frame(mydata[,categorical_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})



################# selecting significant categorical variable using chi square test###########################
mydata_dup<-mydata  #   Creating duplicate data frame and convert the categorical variables to factor so that we can 
                    #   implement chi-square test

#Converting the categorical variables into factor in the duplicated dataset. 
mydata_dup[,categorical_var]<-lapply(mydata_dup[,categorical_var],as.factor)

str(mydata_dup[,categorical_var])

# Chi square test to get the significant categorical variable
p_value_chisq=list()
for (x in 1:length(categorical_var)){
  print(categorical_var[x])
  chisq_test=chisq.test(mydata_dup[,categorical_var[x]],mydata_dup$CHURN)
  p_value_chisq[x]=unlist(chisq_test["p.value"],use.names = FALSE)
}
p_value_chisq=unlist(p_value_chisq,use.names = FALSE)
chisquare_p_value<-cbind(categorical_var,p_value_chisq)
chisquare_p_value<-as.data.frame(chisquare_p_value)
write.csv(chisquare_p_value,"E://Career//R Analytics//Data science Case studies//Logistics regression 4//chisquare_pvalue.csv")



# Below are the variables which are selected after chi-square test
categorical_var_sel<-c("CREDITA",	"CREDITAA",	"CREDITB",	"CREDITC",	"CREDITDE",	"PRIZMRUR",	"PRIZMUB",
               "PRIZMTWN",	"REFURB",	"WEBCAP",	"OWNRENT",	"MARRYUN",	"MAILORD",	"MAILRES",
               "CREDITCD",	"NEWCELLY",	"INCMISS",	"SETPRCM")



################# selecting significant continuous variable using ANOVA test########################
# ANOVA test to get the significant continous variable variable
P_value_anova=list()
for (x in 1:length(continuos_var)){
  a<-aov(mydata[,continuos_var[x]]~factor(mydata$CHURN))
  print(continuos_var[x])
  print(summary(a))
  b<-summary(a)
  P_value_anova[x]=b[[1]][["Pr(>F)"]]
}
P_value_anova<-unlist(P_value_anova,use.names = FALSE)
anova_p_values=cbind(continuos_var,P_value_anova)
anova_p_values<-as.data.frame(anova_p_values)
write.csv(anova_p_values,"E://Career//R Analytics//Data science Case studies//Logistics regression 4//anova_pvalue.csv")
# creditad income refer retaccept retcalls are removed already

##################################################################################################3

############################### Concordance ####################################

# Assuming the input is a stored binomial GLM object
Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0}
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  Somers_D <- conc_rate - disc_rate
  gamma<- (conc_rate - disc_rate)/(conc_rate + disc_rate)
  #k_tau_a<-2*(sum(conc)-sum(disc))/(N*(N-1)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties),
              somers_D=Somers_D, Gamma=gamma))
  
}



######################### Dividing into training and testing dataset ##################################
mydata1=mydata
training=mydata[mydata$CALIBRAT==1,]
testing=mydata[mydata$CALIBRAT==0,]
######################################################################################################


fit1<-glm(CHURN~PHONES +MODELS +	INCALLS+	AGE1+	CHANGEM+	PEAKVCE+		
            MOU+	RECCHRGE+	MOUREC+	OPEAKVCE+	AGE2+	OVERAGE+	CALLWAIT+	
            UNIQSUBS+	DIRECTAS+	UNANSVCE+	CUSTCARE+	REVENUE+	BLCKVCE+DROPBLK+	
            THREEWAY+	ACTVSUBS+	DROPVCE+	ROAM+	INCOME+	EQPDAYS+MONTHS+
            
            CREDITA +CREDITAA +CREDITB+ CREDITC+CREDITDE +PRIZMRUR+ PRIZMUB +PRIZMTWN+REFURB  +WEBCAP+  
            OWNRENT+ MARRYUN  + MAILORD + MAILRES + CREDITCD+NEWCELLY+INCMISS+SETPRCM 
          
          ,data=mydata,family =  binomial("logit")				
)

Concordance(fit1)



###############  Factor analysis on continous variables #########################################

fact_mydata<-mydata[continuos_var]
zv <- apply(fact_mydata, 2, function(x) length(unique(x)) == 1)   # This was done for  the warning which shows
sum(zv)                                                           # some variables have std dev 0
fact_mydata <- fact_mydata[, !zv]                                 # so here I remove the variables that showing std dev 0

corrm<-cor(fact_mydata)


require(psych)
require(GPArotation)


### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

plot.new()
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=T) ### SCREE PLOT
VSS.scree(corrm, main = "scree plot")


require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values)) 



plot(eigen_values$pct_var,type='b')

FA<-fa(r=corrm, 9, rotate="varimax", fm="ml")             
                                           

FA_SORT<-fa.sort(FA)                                

ls(FA_SORT)                                       
FA_SORT$loadings
#FA_SORT$e.values                                         
Loadings<-data.frame(FA_SORT$loadings[1:ncol(fact_mydata),]) 
View(Loadings)
write.csv(Loadings, "E://Career//R Analytics//Data science Case studies//Logistics regression 4//loading10_2.csv") ### SAVING THE FILE



##################################################################################


fit2<-glm(CHURN~ MOU+ INCALLS+ CUSTCARE+ CHANGEM+ MODELS+ RECCHRGE	+				
                DIRECTAS + UNIQSUBS + EQPDAYS + AGE1 + BLCKVCE + OVERAGE	+				
                WEBCAP+	CREDITDE+	SETPRCM+	REFURB+	MAILRES+	MAILORD+
                CREDITAA+	CREDITA+	MARRYUN+	CREDITB+	CREDITC+	INCMISS+
                OWNRENT+	PRIZMRUR+	CREDITCD+	PRIZMTWN+	PRIZMUB
           ,data=training,family =  binomial("logit"))

Concordance(fit2)




############################# Transformations ###################################################
selected_continous_variable<-c("MOU", "INCALLS", "CUSTCARE","MODELS", "RECCHRGE",				
                                "DIRECTAS" , "UNIQSUBS" , "EQPDAYS" , "AGE1" , "BLCKVCE" , "OVERAGE")

t1_square=as.data.frame(lapply(mydata[,selected_continous_variable],function(x){x<-x^2}))
t1_square$CHURN<-mydata1$CHURN
t1_log=as.data.frame(lapply(mydata[,selected_continous_variable],function(x){x<-log(x+1)}))
t1_log$CHURN<-mydata1$CHURN
t1_sqroot=as.data.frame(lapply(mydata[,selected_continous_variable],function(x){x<-sqrt(x)}))
t1_sqroot$CHURN<-mydata1$CHURN
t1_inv=as.data.frame(lapply(mydata[,selected_continous_variable],function(x){x<-(1/(x+1))}))
t1_inv$CHURN<-mydata1$CHURN


for(i in 1:length(selected_continous_variable)){
  print("===================================================================================")
  print(selected_continous_variable[i])
  a1=glm(mydata1[,"CHURN"]~mydata1[,selected_continous_variable[i]],family = binomial("logit"))
  a2=glm(t1_sqroot[,"CHURN"]~t1_sqroot[,selected_continous_variable[i]],family = binomial("logit"))
  a3=glm(t1_square[,"CHURN"]~t1_square[,selected_continous_variable[i]],family = binomial("logit"))
  a4=glm(t1_log[,"CHURN"]~t1_log[,selected_continous_variable[i]],family = binomial("logit"))
  a5=glm(t1_inv[,"CHURN"]~t1_inv[,selected_continous_variable[i]],family = binomial("logit"))
  
  b1=summary(a1)
  b2=summary(a2)
  b3=summary(a3)
  b4=summary(a4)
  b5=summary(a5)
  
  print("Original")
  print(b1$coefficients)
  
  
  print("sqroot")
  print(b2$coefficients)
  
  
  print("square")
  print(b3$coefficients)
  
  
  print("log")
  print(b4$coefficients)
  
  
  print("Inverse")
  print(b5$coefficients)
  
  
}


### Transform variables are written in this manner --transformaion_originalvariablename ##########

mydata1$inv_MOU<-(mydata1$MOU+1)^-1
mydata1$inv_INCALLS<-(mydata1$INCALLS+1)^-1
mydata1$inv_CUSTCARE<-(mydata1$CUSTCARE+1)^-1
mydata1$inv_DIRECTAS<-(mydata1$DIRECTAS+1)^-1
mydata1$sqrt_MODELS<-sqrt(mydata1$MODELS)
mydata1$sq_AGE1<-mydata1$AGE1^2
mydata1$inv_UNIQSUBS<-(mydata1$UNIQSUBS+1)^-1
mydata1$sqrt_EQPDAYS<-sqrt(mydata1$EQPDAYS)
mydata1$inv_BLCKVCE<-(mydata1$BLCKVCE+1)^-1



# SO after making transformation including the transformed variable in training and testing dataset
trainingT<-mydata1[mydata1$CALIBRAT==1,]
testingT<-mydata1[mydata1$CALIBRAT==0,]

fit4<-glm(CHURN ~ inv_MOU+inv_INCALLS+inv_CUSTCARE+CHANGEM+sqrt_MODELS+RECCHRGE	+				
            inv_DIRECTAS+inv_UNIQSUBS+sqrt_EQPDAYS+sq_AGE1+inv_BLCKVCE+OVERAGE	+				
            WEBCAP+	CREDITDE+	SETPRCM+	REFURB+	MAILRES+	MAILORD+
            CREDITAA+	CREDITA+	MARRYUN+	CREDITB+	CREDITC+	INCMISS+
            OWNRENT+	PRIZMRUR+	CREDITCD+	PRIZMTWN+	PRIZMUB
            ,data=trainingT,family =  binomial("logit"))

Concordance(fit4)


require(car)
require(ResourceSelection)
step1<-step(fit4, direction = "both")

fit5<-glm(CHURN ~ inv_MOU + inv_INCALLS + inv_CUSTCARE + CHANGEM + sqrt_MODELS + 
            RECCHRGE + inv_UNIQSUBS + sqrt_EQPDAYS + sq_AGE1 + inv_BLCKVCE +
            OVERAGE + WEBCAP + CREDITDE + REFURB + MAILRES + 
            CREDITA + MARRYUN + CREDITC +  PRIZMUB+PRIZMRUR
          ,data=trainingT,family =  binomial("logit")
)
Concordance(fit5)
hoslem.test(training$CHURN,fitted(fit5),10)
summary(fit5)
vif(fit5)


mydata1$MARRYUN<-factor(mydata1$MARRYUN)
mydata1$WEBCAP<-factor(mydata1$WEBCAP)
mydata1$CREDITDE<-factor(mydata1$CREDITDE)
mydata1$REFURB<-factor(mydata1$REFURB)
mydata1$MAILRES<-factor(mydata1$MAILRES)
mydata1$CREDITA<-factor(mydata1$CREDITA)
mydata1$CREDITC<-factor(mydata1$CREDITC)
mydata1$PRIZMUB<-factor(mydata1$PRIZMUB)
mydata1$PRIZMRUR<-factor(mydata1$PRIZMRUR)

trainingT<-mydata1[mydata1$CALIBRAT==1,]
testingT<-mydata1[mydata1$CALIBRAT==0,]


fit6<-glm(CHURN ~ inv_MOU + inv_INCALLS +  CHANGEM +
            RECCHRGE + inv_UNIQSUBS + sqrt_EQPDAYS + sq_AGE1 + sqrt_MODELS+
            OVERAGE + WEBCAP + CREDITDE + REFURB + MAILRES + inv_CUSTCARE+
             MARRYUN + CREDITC +  PRIZMUB+PRIZMRUR
          ,data=trainingT,family =  binomial("logit")
)
Concordance(fit6)
hoslem.test(training$CHURN,fitted(fit6),10)
summary(fit6)
vif(fit6)



train1<- cbind(trainingT, Prob = predict(fit6, type="response")) 
##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
train1$decile<-factor(train1$decile)

require(dplyr)
decile_grp_train<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp_train, 
                             total_cnt = n() ,
                             min_prob = min(p=Prob),
                             max_prob = max(Prob) ,
                             churn_cnt= sum(CHURN==1),
                             non_churn_cnt = total_cnt -churn_cnt 
)


decile_summ_train<-arrange(decile_summ_train, desc(decile))
View(decile_summ_train)
write.csv(decile_summ_train,"E:\\Career\\R Analytics\\Data science Case studies\\Logistics regression 4\\decile_summ_train_1_11.csv")
View(decile_summ_test)





require(dplyr)
test1<- cbind(testingT, Prob=predict(fit6,testingT, type="response")) 
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))

test1$decile<-factor(test1$decile)

decile_grp_test<-group_by(test1,decile)
decile_summ_test<-summarize(decile_grp_test, 
                            total_cnt = n(), 
                            min_prob = min(p=Prob),
                            max_prob = max(Prob), 
                            churn_cnt= sum(CHURN==1), 
                            non_churn_cnt = total_cnt -churn_cnt )


# SORTING THE DATA IN DESCEINDING  ORDER BASED ON dECILES
decile_summ_test<-arrange(decile_summ_test, desc(decile))
View(decile_summ_test)

write.csv(decile_summ_test,"E:\\Career\\R Analytics\\Data science Case studies\\Logistics regression 4\\decile_summ_test_1_11.csv")
View(decile_summ_test)

#########################################################################################################
### Futher use for offers
Decile_Analysis_categorical<-summarize(decile_grp_train, 
                                       total_cnt = n(), 
                                       min_prob = min(p=Prob),
                                       max_prob = max(Prob), 
                                       churn_cnt= sum(CHURN), 
                                       non_churn_cnt = total_cnt -churn_cnt,
                                       webCap_Per=sum(WEBCAP==1)/total_cnt,
                                       Married_per=sum(MARRYYES==1)/total_cnt,
                                       New_Phone_user_per=sum(NEWCELLY==1)/total_cnt,
                                       CREDIT_B=sum(CREDITB==1)/total_cnt,
                                       CREDIT_C=sum(CREDITC==1)/total_cnt
)

View(Decile_Analysis_categorical)
write.csv(Decile_Analysis_categorical,"E:\\Career\\R Analytics\\Data science Case studies\\Logistics regression 4\\DAC.csv")

##############################################################################################################
Decile_Analysis_Num<-summarize(decile_grp_train, 
                               total_cnt = n(), 
                               min_prob = min(p=Prob),
                               max_prob = max(Prob), 
                               churn_cnt= sum(CHURN), 
                               non_churn_cnt = total_cnt -churn_cnt,
                               Avg_MOU=mean(MOU),
                               Total_Income_per_decile=sum(INCOME),
                               Avg_Recurring_charge=mean(RECCHRGE),
                               Avg_cust_care_call=mean(CUSTCARE),
                               Avg_Month_service=mean(MONTHS),
                               Avg_over_minute_use=mean(OVERAGE),
                               Avg_no_of_unique_subscription=mean(UNIQSUBS),
                               Avg_Eqp_days=mean(EQPDAYS))
View(Decile_Analysis_Num)
write.csv(Decile_Analysis_Num,"E:\\Career\\R Analytics\\Data science Case studies\\Logistics regression 4\\DAN.csv")
###################################################################################################################


###########################################################################################################
require(ROCR)
# AUC Measurement
pred_train<- prediction(train1$Prob, train1$CHURN)
perf_fit <- performance(pred_train, "tpr", "fpr")
plot(perf_fit,main="ROC CURVE")
abline(0, 1)
performance(pred_train, "auc")@y.values

#############################################################################################################
######## Deciding the cut-off###########################################################################
require(caret)
threshold=seq(0.21 ,0.80, by= 0.01)
sensitivity_=list()
specificity_=list()
j=1
for(i in seq(0.21 ,0.80, by= 0.01) ){
  predicted_values<-ifelse(predict(fit6,type="response")>i,1,0)
  actual_values<-fit6$y
  conf_matrix<-table(predicted_values,actual_values)
  sensitivity_[j]=sensitivity(conf_matrix)
  specificity_[j]=specificity(conf_matrix)
  j=j+1
}

sensitivity_=unlist(sensitivity_, use.names=FALSE)
specificity_=unlist(specificity_,use.names = FALSE)
xyz=cbind(threshold,sensitivity_,specificity_)
xyz=as.data.frame(xyz)
write.csv(xyz,"E://Career//R Analytics//Data science Case studies//Logistics regression 4//sensi_specif.csv")


# Final prediction of churn or not churn from cut-off probabilities
train1$predicted_values<-ifelse(train1$Prob>0.50,1,0) 
test1$predicted_values<-ifelse(test1$Prob>0.50,1,0)
mean(train1$CHURN==train1$predicted_values)
mean(test1$CHURN==test1$predicted_values)
ALLDATA<-rbind(train1,test1)
View(ALLDATA)


## Now based on probabilities we segment the telecom customer into 4 categories which tells us which segment
#of customers is more  prone to risk of churn 

#Following probabilities cut-off are taken after considerably looking at decile analysis

# BAD CUSTOMERS

Segment1<-ALLDATA[ALLDATA$Prob>0.6,]

# NOT GOOD Customers

Segment2<-ALLDATA[ALLDATA$Prob>0.5 & ALLDATA$Prob<=0.6,]


# GOOD CUSTOMERS

segment3<-ALLDATA[ALLDATA$Prob<=0.5 & ALLDATA$Prob>0.35,]

# VERY GOOD CUSTOMERS

segment4<-ALLDATA[ALLDATA$Prob<=0.35,]



# Variable importance

varIMP<-varImp(fit6)
write.csv(varIMP,"E://Career//R Analytics//Data science Case studies//Logistics regression 4//std_coeff.csv")



