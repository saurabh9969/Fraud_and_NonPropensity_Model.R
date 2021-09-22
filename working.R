# Fraud_and_NonPropensity_Model.R

#* @param policy_number 
#* @param age
#* @param billable_frequency
#* @param policy_term
#* @param channel
#* @param marital_status
#* @param APE
#* @param salary
#* @param payment_method
#* @param qualification
#* @param occupation
#* @param gender
#* @param APE_salary
#* @param nominee_relationship
#* @param age_proof
#* @param ECS_mandate
#* @param sum_assured_APE
#* @param application_month
#* @param application_day_of_week
#* @param application_day_ofmonth
#* @param state
#* @post /PROD_FraudPredict

function(policy_number,age,billable_frequency,policy_term,channel,marital_status,APE,salary,payment_method,qualification,occupation,gender,APE_salary,nominee_relationship,age_proof,ECS_mandate,sum_assured_APE,application_month,application_dayofmonth,application_day_of_week,state)
{
  library(randomForest)  
  library(e1071)
  library(odbc)


  fn_tag <- "PROD_FRAUD"
  parameters <- paste("age:",age,",billable_frequency:",billable_frequency,",policy_term:",policy_term,",channel:",channel,",marital_status:",marital_status,",salary:",salary,",payment_method:",payment_method,",qualification:",qualification,",occupation:",occupation,",gender:",gender,",nominee_relationship:",nominee_relationship,",age_proof:",age_proof,",ECS_mandate:",ECS_mandate,",sum_assured_APE:",sum_assured_APE,",application_month:",application_month,",application_dayofmonth:",application_dayofmonth,",application_day_of_week:",application_day_of_week,",state:",state,",APE_salary:",APE_salary,",APE:",APE)    
  
  result = tryCatch({
    expr =
      NEWdata <- data.frame(
        POLICY_NUMBER = as.integer(policy_number),
        AGE_BAND = age,
        BILLING_FREQUENCY = billable_frequency,
        policy_term1= policy_term,
        channel1 = channel,
        marital1 = marital_status,
        APE_BAND = APE,
        SALARY_BAND = salary,
        PAYMENT_METHOD = payment_method,
        qual1 = qualification,
        occ1 = occupation,
        SEX = gender,
        APESALARY_BAND = APE_salary,
        NOMINEE_RELATION_Desc = nominee_relationship,
        AGE_PROOF = age_proof,
        ECS_Mandate = ECS_mandate,
        SA_APE_BAND = sum_assured_APE,
        Application_Month = application_month,
        Application_DayofWeek = application_day_of_week,
        Application_DayofMonth = application_dayofmonth,
        STATE_CLEAN = state
      )
    
    Fraud_Base<- read.csv("Fraud_Base.CSV")
    
    Fraud_Base$ECS_Mandate <- as.factor(Fraud_Base$ECS_Mandate)
    Fraud_Base$Application_Month <- as.factor(Fraud_Base$Application_Month)
    Fraud_Base$Application_DayofMonth <- as.factor(Fraud_Base$Application_DayofMonth)
    
    NEWdata <- rbind(Fraud_Base[1,1:(length(Fraud_Base)-1)], NEWdata)
    NEWdata <- NEWdata[-1,]
  
    #model1_F <- glm (Frauds~.-POLICY_NUMBER - Application_DayofMonth- BILLING_FREQUENCY- marital1-PAYMENT_METHOD, data=Fraud_Base, family = binomial) # Better One 
    #saveRDS(model1_F, "fraud_glm.rds")
    fraud_glm<- readRDS("fraud_glm.rds")
    summary(fraud_glm)
    predict1_F <- predict(fraud_glm, NEWdata, type = 'response')
    summary(predict1_F)
    
    #model2_F <- randomForest (Frauds~.-POLICY_NUMBER ,ntree = 100,data = Fraud_Base, na.action=na.exclude,importance = TRUE)
    #summary(model2_F)
    #saveRDS(model2_F, "fraud_rf.rds")
    fraud_rf<- readRDS("fraud_rf.rds")
    summary(fraud_rf)
    predict2_F <- predict(fraud_rf,NEWdata)
    
    
    Final_F <- 0.7*predict1_F + 0.3*predict2_F 
    Final_F_HML <- ifelse(Final_F > 0.25, "High", ifelse(Final_F > 0.05,"Medium", "Low"))
    

    
    resultframe <- data.frame(
      policy_number =  policy_number,
      Fraud_propensity = Final_F_HML,
      probability = Final_F,
      result= "SUCCESS"
    )
  
    

    result=tryCatch({
    storetodb(policy_number,fn_tag ,parameters,"SUCCESS",Final_F, Final_F_HML)
    return (resultframe)
    },finally= {
      return (resultframe)
})


  }, error = function(error_condition) {
    
    resultframe <- data.frame(
      policy_number =  policy_number,
      Fraud_propensity = "NULL",
      probability = "NULL",
      result = geterrmessage()
    )


   
    result=tryCatch({ 
    storetodb(policy_number,fn_tag ,parameters,geterrmessage(),'NULL', 'NULL')
    return (resultframe)
    },finally= {
      return (resultframe)
})


  }
  # , 
  # finally = function (response)
  #     {
  #     return (resultframe)}
  )
}



#* Return the predication value for non Disclosure model
#* @param policy_number 
#* @param age
#* @param billable_frequency
#* @param policy_term
#* @param channel
#* @param marital_status
#* @param APE
#* @param salary
#* @param payment_method
#* @param qualification
#* @param occupation
#* @param gender
#* @param APE_salary
#* @param nominee_relationship
#* @param age_proof
#* @param ECS_mandate
#* @param sum_assured_APE
#* @param application_month
#* @param application_day_of_week
#* @param application_dayofmonth
#* @param state
#* @post /PROD_NonpropensityPredict

function(policy_number,age,billable_frequency,policy_term,channel,marital_status,APE,salary,payment_method,qualification,occupation,gender,APE_salary,nominee_relationship,age_proof,ECS_mandate,sum_assured_APE,application_dayofmonth,application_month,application_day_of_week,state)
{
  
  library(randomForest)  
  library(e1071) 
  fn_tag <- "PROD_NONDISCLOSURE"
  parameters <- paste("age:",age,",billable_frequency:",billable_frequency,",policy_term:",policy_term,",channel:",channel,",marital_status:",marital_status,",salary:",salary,",payment_method:",payment_method,",qualification:",qualification,",occupation:",occupation,",gender:",gender,",nominee_relationship:",nominee_relationship,",age_proof:",age_proof,",ECS_mandate:",ECS_mandate,",sum_assured_APE:",sum_assured_APE,",application_month:",application_month,",application_dayofmonth:",application_dayofmonth,",application_day_of_week:",application_day_of_week,",state:",state,",APE_salary:",APE_salary,",APE:",APE)  
  
  result = tryCatch({
    expr =  
      NEWdata <- data.frame(
        AGE_BAND = age,
        BILLING_FREQUENCY = billable_frequency,
        policy_term1= policy_term,
        channel1 = channel,
        marital1 = marital_status,
        APE_BAND = APE,
        SALARY_BAND = salary,
        PAYMENT_METHOD = payment_method,
        qual1 = qualification,
        occ1 = occupation,
        SEX = gender,
        APESALARY_BAND = APE_salary,
        NOMINEE_RELATION_Desc = nominee_relationship,
        AGE_PROOF = age_proof,
        ECS_Mandate = ECS_mandate,
        SA_APE_BAND = sum_assured_APE,
        Application_Month = application_month,
        Application_DayofWeek = application_day_of_week,
        Application_DayofMonth = application_dayofmonth,
        STATE_CLEAN = state
      )
    
    ND_Base<- read.csv("ND_Base.CSV")
    ND_Base$ECS_Mandate <- as.factor(ND_Base$ECS_Mandate)
    ND_Base$Application_Month <- as.factor(ND_Base$Application_Month)
    ND_Base$Application_DayofMonth <- as.factor(ND_Base$Application_DayofMonth)
    
    
    NEWdata <- rbind(ND_Base[1,1:(length(ND_Base)-1)],NEWdata)
    NEWdata <- NEWdata[-1,]
    
    #model1_ND <- glm (Non_Disclosure~.- Application_DayofMonth-marital1, data=ND_Base, family = binomial) # Better One
    #saveRDS(model1_ND, "ng_glm.rds")
    nd_glm<- readRDS("nd_glm.rds")
    #model2_ND = randomForest(Non_Disclosure~.,ntree = 100,data = ND_Base, na.action=na.exclude, importance = TRUE, maxnodes = 12)
    #saveRDS(model2_ND, "nd_rf.rds")
    nd_rf<- readRDS("nd_rf.rds")
    predict1_ND <- predict(nd_glm, NEWdata, type = 'response')
    predict2_ND  <- predict(nd_rf, NEWdata)
    
    Final_ND <- 0.7*predict1_ND + 0.3*predict2_ND
    Final_ND_HML <- ifelse(Final_ND > 0.25, "High", ifelse(Final_ND > 0.05,"Medium", "Low"))
    
    
    resultframe <- data.frame(
      policy_number =  as.character(policy_number),
      NonPropensity = Final_ND_HML,
      probability = Final_ND,
      result= "SUCCESS"
    )
 


    result=tryCatch({
    storetodb(policy_number,fn_tag ,parameters,"SUCCESS",Final_ND, Final_ND_HML)
    return (resultframe)
    },finally = {
      return(resultframe)
})



    
  }, error = function(error_condition) {
    
    resultframe <- data.frame(
      policy_number =  policy_number,
      NonPropensity = "NULL",
      probability = "NULL",
      result = geterrmessage()
    )
 


    result=tryCatch({
    storetodb(policy_number,fn_tag ,parameters,geterrmessage(),'NULL', 'NULL')    
    return (resultframe)
    },finally = {
      return(resultframe)
})


  }
  # , 
  # finally = function (response)
  # {
  #   return (resultframe)}
  )
}


# this stroreDB method will store the data to database table.

storetodb <- function(policy_number,fn_tag,parameters,resultstr,probability_value, level_value)
{
  library(odbc)
 #  library(RODBC)
  con <- DBI::dbConnect(odbc(),
                        Driver = "ODBC Driver 17 for SQL Server",
                        Server = "10.1.6.213",
                        Database = "barolgrds",
                        UID = "rai_read",
                        PWD = "rai@read",
                        Port = 1433)
  
  noofrecords = DBI::dbGetQuery(con,'select count(*) from barolgrds.RAI.RPROD_NB_FRAUDNONDISCLOSURE')
  
  requestid= noofrecords+1
 Sys.setenv(TZ='IST')

 reqdate <- Sys.time()
  
  # Create the query statement
  query<-paste(
    "INSERT INTO barolgrds.RAI.RPROD_NB_FRAUDNONDISCLOSURE   VALUES(",requestid,",'",policy_number,"','",fn_tag,"','",reqdate,"','",parameters,"','",resultstr,"',",probability_value,",'",level_value,"')"
  )
  #print(query)
  
  #excute a query
  queryresult = DBI::dbGetQuery(con,query)    
  
  #disconnect a connection
  dbDisconnect(con)
  
}

