#This file contains samples of the code that I wrote when I was a data science intern 
#at the Cook County Assessor's Office. Please note that this code will not run. It is fragments I wrote
#of a larger algorithm that is used to assess the property value of every property within a given
#township in Cook County. The entire code used by the Assessor's Office is publically available
#at https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#FIRST EXAMPLE: below is code used to create a function list that efficiently recode variables. 
#This is contained in the "utility" file in the ccao_utility repository

#efficient recoding function list
e.recode <- list(
  #Function to recode numeric columns:
  num = function(df, columns){
    for(a in 1:length(columns)){
      #Use grep to create an numeric object where the column name matches in the dataset
      b <- grep(paste("^",columns[a],"$", sep=""),names(df))
      if(length(b)==0){
        stop(paste(columns[a], "column not in dataset, cannot convert to numeric", sep = ""))
      }else{
        df[,b] <- as.numeric(as.character(df[,b]))
      }}
    df
  },
  #function to recode factor columns:
  fac = function(df, columns){
    for(a in 1:length(columns)){
      b <- grep(paste("^",columns[a],"$", sep=""),names(df))
      if(length(b)==0){
        stop(paste(columns[a], "column not in dataset, cannot convert to factor", sep = ""))
      }else{
        df[,b] <- as.factor(df[,b])
      }}
    df
  },
  #function to recode date columns:
  date = function(df, columns){
    for(a in 1:length(columns)){
      b <- grep(paste("^",columns[a],"$", sep=""),names(df))
      if(length(b)==0){
        stop(paste(columns[a], "column not in dataset, cannot convert to Date", sep = ""))
      }else{
        df[,b] <- as.Date(df[,b])
      }}
    df
  },
  #function to recode character columns: 
  char = function(df, columns){
    for(a in 1:length(columns)){
      b <- grep(paste("^",columns[a],"$", sep=""),names(df))
      if(length(b)==0){
        stop(paste(columns[a], "column not in dataset, cannot convert to character", sep = ""))
      }else{
        df[,b] <- as.character(df[,b])
      }}
    df
  }
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SECOND EXAMPLE: from the files 0_build_200class_modeldata.R and 0_build_200class_modeldata.R, below is the code
#used to come up with the price tercile probit model used to estimate which tercile which tercile 
#every property in the townshipwould fall into. 

#code from 0_build_200class_modeldata.R: 
modeldata$price_tercile <- NA
tercile_info <- NULL

for(m in levels(modeldata$modeling_group)){
  onethird <- quantile(modeldata$sale_price[modeldata$modeling_group == m & 
                                              !is.na(modeldata$sale_price) & 
                                              modeldata$sale_price>100], c(.33), names = FALSE, na.rm = TRUE)
  twothird <- quantile(modeldata$sale_price[modeldata$modeling_group == m &
                                              !is.na(modeldata$sale_price) &
                                              modeldata$sale_price>100], c(.66), names = FALSE, na.rm = TRUE)
  modeldata$price_tercile[modeldata$sale_price <= onethird & modeldata$modeling_group == m] <- 1
  modeldata$price_tercile[modeldata$sale_price > onethird & modeldata$sale_price <= twothird & modeldata$modeling_group == m] <- 2
  modeldata$price_tercile[modeldata$sale_price > twothird & modeldata$modeling_group == m] <- 3 
  tercile_info <- rbind(tercile_info, c(m, onethird, twothird))
}
modeldata$price_tercile[modeldata$sale_price<=100] <- NA
modeldata$price_tercile <- factor(modeldata$price_tercile)
modeldata$price_tercile <- ordered(modeldata$price_tercile, c(1, 2, 3))
colnames(tercile_info) <- c("modeling_group", "lower_third_cutoff", "upper_third_cutoff")

#tercile integrity checks
m_tercile_integ <- modeldata[,-10] %>% 
  dplyr::group_by(modeling_group, price_tercile) %>% 
  dplyr::summarise(count = n(), median.sale.price = median(sale_price, na.rm=TRUE))

#create an ordered logit model to predict price tercile in unsold data
#Specify model parameters:
tercile_models <- read.xlsx("ccao_dictionary_draft.xlsx", sheetName = "price_tercile_model", stringsAsFactors=FALSE)
tercile_models <- tercile_models[tercile_models$township == target_township, ]
sfmod <- tercile_models$sf_probit_model
mfmod <- tercile_models$mf_probit_model
#ncmod <- tercile_models$nchars_probit_model

err <- try(TERC_MOD_SF <- polr(formula = sfmod, data = modeldata, subset= modeldata$modeling_group=="SF", method = "probit", na.action = na.omit))
if(class(err)!="try-error"){
  TERC_MOD_SF <- polr(formula = sfmod, data = modeldata, subset= modeldata$modeling_group=="SF", method = "probit", na.action = na.omit)
}
integ_check_f(class(err)!="try-error",
              paste0("Price Tercile Probit Model for SF"),
              paste0("Good: Model Created for SF"),
              paste0("Bad: Model Not Created for SF"),
              "0")

err <- try(TERC_MOD_MF <- polr(formula = mfmod, data = modeldata, subset= modeldata$modeling_group=="MF", method = "probit", na.action = na.omit))
if(class(err)!="try-error"){
  TERC_MOD_MF <- polr(formula = mfmod, data = modeldata, subset= modeldata$modeling_group=="MF", method = "probit", na.action = na.omit)
}
integ_check_f(class(err)!="try-error",
              paste0("Price Tercile Probit Model for MF"),
              paste0("Good: Model Created for MF"),
              paste0("Bad: Model Not Created for MF"),
              "0")
# previous integrity check only created tercile probit model for mf when condition met
# if(class(err)!="try-error"){
#   TERC_MOD_MF <- polr(formula = mfmod, data = modeldata, subset= modeldata$modeling_group=="MF", method = "probit", na.action = na.omit)
#   check<- paste0("Price Tercile Probit Model for MF")
#   msg<- paste0("Good: Model Created for MF")
#   integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
#   print(check); print(msg); rm(check, msg)
# }else{
#   check<- paste0("Price Tercile Probit Model for MF")
#   msg<- paste0("Bad: Model Not Created for MF")
#   integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
#   print(check); print(msg); rm(check, msg)
# }

#err <- try(TERC_MOD_NCHARS <- polr(formula = ncmod, data = modeldata, subset= modeldata$modeling_group=="NCHARS", method = "probit", na.action = na.omit))
#if(class(err)!="try-error"){
#  TERC_MOD_NCHARS <- polr(formula = ncmod, data = modeldata, subset= modeldata$modeling_group=="NCHARS", method = "probit", na.action = na.omit)
#  check<- paste0("Price Tercile Probit Model for NCHARS")
#  msg<- paste0("Good: Model Created for NCHARS")
#  integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
#  print(check); print(msg); rm(check, msg)
#}else{
#  check<- paste0("Price Tercile Probit Model for NCHARS")
#  msg<- paste0("Bad: Model Not Created for NCHARS")
#  integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
#  print(check); print(msg); rm(check, msg)
#}

remove(sfmod, mfmod) #need to add ncmod to this list if using NCHARS model

#Check tercile prediction accuracy: 
err <- try(predict(TERC_MOD_SF, newdata = subset(modeldata, subset = modeldata$modeling_group == "SF")))
if(class(err)!="try-error"){
  modeldata$price_tercile_pred[modeldata$modeling_group=="SF"] <- predict(TERC_MOD_SF, newdata = subset(modeldata, subset = modeldata$modeling_group == "SF"))
}
err <-try(predict(TERC_MOD_MF, newdata = subset(modeldata, subset = modeldata$modeling_group == "MF")))
if(class(err)!="try-error"){
  modeldata$price_tercile_pred[modeldata$modeling_group=="MF"] <- predict(TERC_MOD_MF, newdata = subset(modeldata, subset = modeldata$modeling_group == "MF"))
}
#err <- try(predict(TERC_MOD_NCHARS, newdata = subset(modeldata, subset = modeldata$modeling_group == "NCHARS")))
#if(class(err)!="try-error"){
#  modeldata$price_tercile_pred[modeldata$modeling_group=="NCHARS"] <- predict(TERC_MOD_NCHARS, newdata = subset(modeldata, subset = modeldata$modeling_group == "NCHARS"))
#}
modeldata$price_tercile_pred <- ordered(modeldata$price_tercile_pred, c(1, 2, 3))
modeldata$price_tercile_pred[modeldata$sale_price<=100] <- NA

ptacc <- round(mean((modeldata$price_tercile == modeldata$price_tercile_pred), na.rm = TRUE)*100)
pt1acc <- round(mean((modeldata$price_tercile[modeldata$price_tercile=="1"] == modeldata$price_tercile_pred[modeldata$price_tercile=="1"]), na.rm = TRUE)*100)
pt2acc <- round(mean((modeldata$price_tercile[modeldata$price_tercile=="2"] == modeldata$price_tercile_pred[modeldata$price_tercile=="2"]), na.rm = TRUE)*100)
pt3acc <- round(mean((modeldata$price_tercile[modeldata$price_tercile=="3"] == modeldata$price_tercile_pred[modeldata$price_tercile=="3"]), na.rm = TRUE)*100)

check<- paste0("Accuracy for price tercile probit model")
msg<- paste0(" Overall: ",ptacc, "% 1st tercile: ", pt1acc, "% 2nd tercile: ", pt2acc, "% 3rd tercile: ", pt3acc, "%")
integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
print(check); print(msg)
rm(check, msg, ptacc, pt1acc, pt2acc, pt3acc, err, onethird, twothird)

#Uncomment this code to create a plot to visualize price tercile prediction on model data: 
#tercile_info<-as.data.frame(tercile_info)
#sflow <- as.numeric(as.character(tercile_info[3,2]))
#sfhigh <- as.numeric(as.character(tercile_info[3,3]))
#mflow <- as.numeric(as.character(tercile_info[1,2]))
#mfhigh <- as.numeric(as.character(tercile_info[1,3]))
##include the following lines for nchar info only:
##nclow <- as.numeric(as.character(tercile_info[2,2]))
##nchigh <- as.numeric(as.character(tercile_info[2,3])) 

#ggplot(data = subset(modeldata, !is.na(price_tercile_pred)), aes(x = sale_price)) + 
#  geom_density(aes(fill=price_tercile_pred), position="fill") + 
#  coord_cartesian(xlim=c(0, 1500000)) + scale_color_brewer(palette="Set2") +
#  labs(title = "Density Plot of Price Tercile Prediction", x = "Sale Price") +
#  theme(legend.position="bottom") +
#  geom_vline(xintercept= c(sflow, sfhigh), linetype="dashed", color = "red") +
#  geom_vline(xintercept= c(mflow, mfhigh), linetype="longdash", color = "royalblue") +
#  #geom_vline(xintercept= c(nclow, nchigh), linetype="dotted", color = "magenta") + #include this line for nchars only
#  annotation_custom(grob=(grid.text("Red lines = SF Terciles; Blue Lines = MF terciles", x=.7, y=.2, gp=gpar(col="black", fontsize=11))))
#remove(sflow, sfhigh, mflow, mfhigh) #add nclow, nchigh to this list if using nchars

modeldata$price_tercile_pred <- NULL

#code from 0_build_200class_modeldata.R: 

colnames(assmntdata)[colnames(assmntdata)=="most_recent_sale_price"] <- "sale_price"
assmntdata$LOC_PRED <- as.numeric(predict(LOCF_MODEL,newdata=assmntdata))
colnames(assmntdata)[colnames(assmntdata)=="sale_price"] <- "most_recent_sale_price"
assmntdata$LOCF1 <- assmntdata$LOC_PRED/mean(assmntdata$LOC_PRED)

# generate combined neighborhood/towncode variable specific to modeling groups to avoid having to use interaction terms
assmntdata$town_nbhd <- as.factor(paste0(assmntdata$TOWN_CODE, assmntdata$NBHD))

assmntdata$modeling_group <- as.factor(assmntdata$modeling_group)

for (i in levels(assmntdata$modeling_group)){
  eval(parse(text = paste0("assmntdata$town_nbhd_", i, " <- assmntdata$town_nbhd")))
  eval(parse(text = paste0("assmntdata$town_nbhd_", i, "[assmntdata$assmnting_group!='", i, "']<-NA")))
}

#Predict price tercile using ordered logic model
assmntdata$price_tercile <- NA
for(m in levels(assmntdata$modeling_group)){
  model <- paste("TERC_MOD_", m, sep = "")
  err <- try(predict(object=eval(parse(text=model)), newdata=subset(assmntdata, modeling_group==m), na.action = na.exclude))
  if(class(err)!="try-error"){
    assmntdata$price_tercile[assmntdata$modeling_group==m] <- 
      predict(object=eval(parse(text=model)), newdata=subset(assmntdata, modeling_group==m), na.action = na.exclude)
    check<- paste0("Price Tercile for ", paste(m), " in Assmntdata")
    msg<- paste0("Good: Price Tercile assigned")
    integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
    print(check); print(msg); rm(check, msg)
  }else{
    check<- paste0("Price Tercile for ", paste(m), " in Assmntdata")
    msg<- paste0("Bad: Price Tercile not assigned")
    integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
    print(check); print(msg); rm(check, msg)
  }
}
assmntdata$price_tercile<-as.factor(assmntdata$price_tercile)
assmntdata$price_tercile<-ordered(assmntdata$price_tercile, c(1, 2, 3))

#tercile integrity checks
integ_check_f(anyNA(assmntdata$price_tercile)==TRUE,
              paste0("All assessment PINs have price tercile value?"),
              paste0("Bad: No, some PINs don't have price tercile value"),
              paste0("Good: All PINs have price tercile value"),
              "0")

a_tercile_integ <- assmntdata[,-8] %>% 
  dplyr::group_by(modeling_group, price_tercile) %>% 
  dplyr::summarise(count = n(), median.sale.price = median(most_recent_sale_price, na.rm=TRUE))
a_tercile_integ <- as.data.frame(a_tercile_integ)
mfsum <- a_tercile_integ %>% filter(modeling_group=="MF")
sfsum <- a_tercile_integ %>% filter(modeling_group=="SF")
#ncsum <- a_tercile_integ %>% filter(modeling_group=="NCHARS")

integ_check_f(is.unsorted(mfsum$median.sale.price),
              paste0("Do MF price tercile median sale values increase in each tercile?"),
              paste0("Bad: median price does not increase"),
              paste0("Good: median price increases"),
              "0")
integ_check_f(is.unsorted(sfsum$median.sale.price),
              paste0("Do SF price tercile median sale values increase in each tercile?"),
              paste0("Bad: median price does not increase"),
              paste0("Good: median price increases"),
              "0")

#if(is.unsorted(ncsum$median.sale.price)){
#  check<- paste0("Do NCHARS price tercile median sale values increase in each tercile?")
#  msg<- paste0("Bad: median price does not increase")
#  integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
#  print(msg); rm(check, msg)
#}else{
#  check<- paste0("Do NCHARS price tercile median sale values increase in each tercile?")
#  msg<- paste0("Good: median price increases")
#  integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg))
#  print(msg); rm(check, msg)
#}

remove(mfsum, sfsum) #add ncsum to this list if using NCHARS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#THIRD EXAMPLE: below is the code written to implement several different statistical models 
#that could be used to predict the value of every property in a given township. This code will implement
#Quantile Regression, Ordinary Least Square (OLS), and Gradient Boosting Machine models that are taken
#from an Excel workbook (ccao_dictionary_draft.xlsx) that contains hundreds of models to try.
#The complete loop (the code of which is not included here) is designed to select the best and most 
#accurate model to use in the final valuation of properties. 

models <- subset(
  read.xlsx("ccao_dictionary_draft.xlsx", sheetName = "models", stringsAsFactors=FALSE)
  , estimation_method %in% c("OLS", "Quantile", "GBM")
  # & modeling_group=="NCHARS"
)

if(models$estimation_method[m]=="Quantile" 
   & (models$type[m]=="lin-lin" | models$type[m]=="log-log")){
  model <- try(rq(as.formula(models$form[m])
                  ,data=subset(modeldata, filter_1>=filter_setting_1 & modeling_group==models$modeling_group[m])
                  , tau=.5))
}

if(models$estimation_method[m]=="GBM"
   & (models$type[m]=="lin-lin" | models$type[m]=="log-log")){
  model <- try(gbm(as.formula(models$form[m])
                   ,distribution="gaussian"
                   ,interaction.depth=2
                   ,n.trees=ntree
                   ,cv.folds = 1
                   ,data=subset(modeldata, filter_1>=filter_setting_1 & modeling_group==models$modeling_group[m])))
}

if(class(model)=="rq"){
  summary(model, se = "iid")
}else{
  summary(model)
}

if((models$estimation_method[m]=="OLS" | models$estimation_method[m]=="Quantile") & models$type[m]=="log-log"){
  predictions <- NA
  TF <- modeldata$modeling_group==models$modeling_group[m] & modeldata$filter_1 >= 1
  error <-  try(exp(predict(model, newdata=modeldata[TF,])))
  if(class(error)!="try-error"){
    predictions[TF] <-  try(exp(predict(model, newdata=modeldata[TF,])))
    modeldata$fitted_value <- predictions
    print("Values predicted")
  }else{
    if(filter_setting_1==1){
      print(error)  
      check<- paste0("Able to estimate all models with no trimming?")
      msg<- paste0("Bad: failed to predict values for ",models$model_id[m], " at trim setting 1")
      stage<- paste0("2")
      integrity_checks<- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg, stage=stage))
      print(check); print(msg); rm( msg)
      next
    }else{
      print(paste0("Error predicting values, skipping loop."))
      next
    }
  }
}

if((models$estimation_method[m]=="OLS" | models$estimation_method[m]=="Quantile") & models$type[m]=="lin-lin"){
  predictions <- NA
  TF <- modeldata$modeling_group==models$modeling_group[m] & modeldata$filter_1 >= 1
  
  error <- tryCatch({
    predict(model, newdata=modeldata[TF,])
  }, error=function(e){return(e)}
  )
  
  if(class(error)[1]!="simpleError"){
    predictions[TF] <- try(predict(model, newdata=modeldata[TF,]))
    modeldata$fitted_value <- predictions
    print("Values predicted")
  }else{
    if(filter_setting_1==1){
      print(error)  
      check<- paste0("Able to estimate all models with no trimming?")
      msg<- paste0("Bad: failed to predict values for ",models$model_id[m], " at trim setting 1")
      stage<- paste0("2")
      integrity_checks <- rbind.fill(integrity_checks, data.frame(check=check, outcome=msg, stage=stage))
      print(check); print(msg); rm( msg)
      next
    }else{
      print(paste0("Error predicting values, skipping loop."))
      next
    }
  }
}

if (models$estimation_method[m]=='GBM' & models$type[m] == 'lin-lin'){
  predictions <- NA
  TF <- modeldata$modeling_group==models$modeling_group[m] & modeldata$filter_1 >= 1
  error <- try(predict(model, newdata=modeldata[TF,], n.trees=ntree))
  if (class(error) != "try-error"){
    predictions[TF] <- try(predict(model, newdata=modeldata[TF,], n.trees=ntree))
    modeldata$fitted_value <- predictions
    print("Values predicted")
  }
  else{
    if(filter_setting_1==1){
      print(error)
      check<- paste0("Able to estimate all models with no trimming?")
      msg<- paste0("Bad: failed to predict values for ",models$model_id[m], " at trim setting 1")
      integrity_checks <- rbind(integrity_checks, data.frame(check=check, outcome=msg))
      print(check); print(msg); rm( msg)
      next
    }else{
      print(paste0("Error predicting values, skipping loop."))
      next
    }
  }
}

if (models$estimation_method[m]=='GBM' & models$type[m] == "log-log"){
  predictions <- NA
  TF <- modeldata$modeling_group==models$modeling_group[m] & modeldata$filter_1 >= 1
  error <- try(exp(predict(model, newdata=modeldata[TF,], n.trees=ntree)))
  if (class(error) != "try-error"){
    predictions[TF] <- try(exp(predict(model, newdata=modeldata[TF,], n.trees=ntree)))
    modeldata$fitted_value <- predictions
    print("Values predicted")
  }
  else{
    if(filter_setting_1==1){
      print(error)
      check<- paste0("Able to estimate all models with no trimming?")
      msg<- paste0("Bad: failed to predict values for ",models$model_id[m], " at trim setting 1")
      integrity_checks <- rbind(integrity_checks, data.frame(check=check, outcome=msg))
      print(check); print(msg); rm( msg)
      next
    }else{
      print(paste0("Error predicting values, skipping loop."))
      next
    }
  }
}

r <- try(summary(model)$adj.r.squared)
if(is.null(r)|class(r)=="try-error"){r<-NA}

if(models$estimation_method[m]=="OLS"){
  n <- nobs(model)
}else if(models$estimation_method[m]=="Quantile"){
  n <-length(model[["residuals"]])
}else if(models$estimation_method[m]=="GBM"){
  n <- length(model[["fit"]])
}else{
  n<-NA
}






