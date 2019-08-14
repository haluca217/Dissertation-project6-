
setwd("C:/Users/haruk/Desktop/Dissertation/2nd_Project/CODE/WorkDirectory")

library(excel.link)
library(dplyr)
library(ggplot2)
library(ggsci)
library(scales)
library(corrplot)
library(corrr)
library(kernlab)
library(caret)
library(randomForest)
library(nnet)
library(klaR)
library(pROC)
library(rminer)
library(RColorBrewer)


library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)




##data read

##replace ******* to password
dat <- xl.read.file("../../DATA/synthetic_hospitaldata_V1_4.xlsx", password = "*******")


dat_group <- dat %>% 
  mutate(fake_period = ifelse(.$fake_age < 10 , "0~", 
                         ifelse(.$fake_age < 20 , "10~", 
                            ifelse(.$fake_age < 30 , "20~", 
                              ifelse(.$fake_age < 40 , "30~",
                                ifelse(.$fake_age < 50 , "40~", 
                                  ifelse(.$fake_age < 60 , "50~",
                                    ifelse(.$fake_age < 70 , "60~",
                                      ifelse(.$fake_age < 80 , "70~","80~"))))))))) %>% 
  mutate(fake_period = factor(.$fake_period, levels = c("0~", "10~", "20~", "30~", "40~", "50~", "60~", "70~", "80~"), ordered = TRUE))

dat_group <- dat_group %>% 
  mutate(fake_stay = ifelse(.$fake_los < 1,"0", 
                       ifelse(.$fake_los  < 5 ,"1~", 
                         ifelse(.$fake_los < 10 , "5~",
                           ifelse(.$fake_los < 20, "10~", 
                             ifelse(.$fake_los < 30, "20~", "30~")))))) %>% 
  mutate(fake_stay = factor(.$fake_stay, levels = c("0", "1~", "5~", "10~", "20~", "30~"), ordered = TRUE))

dat_group <- dat_group %>% 
  mutate(fake_type = ifelse(.$fake_admission <= 19 ,"Routine Admission", 
                       ifelse( .$fake_admission  <= 22 ,"Urgent Admission", 
                         ifelse( .$fake_admission <= 39 , "Emergency Admission",
                           ifelse( .$fake_admission <= 48 , "Other Admission", "Not Known"))))) %>% 
  mutate(fake_type = factor(.$fake_type, levels = c("Routine Admission", "Urgent Admission", "Emergency Admission", "Other Admission", "Not Known"), ordered = TRUE))

#####after checking bargraph

dat_group <- dat_group %>% 
  mutate(fake_admission2 = ifelse(.$fake_type == "Urgent Admission" | .$fake_type == "Emergency Admission", "Urgent & Emergency Admission", "Routine Admission" 
  )) %>% 
  mutate(fake_admission2 = factor(.$fake_admission2, levels = c("Urgent & Emergency Admission", "Routine Admission"), ordered = TRUE))



dat_group <- mutate(dat_group, fake_diag = dat$fake_shortdiag)


for (i in 1:length(dat_group$fake_diag)){
  switch(substr(dat_group$fake_shortdiag[i], 1, 1),               
         "A" = dat_group$fake_diag[i] <- paste0("1", substr(dat_group$fake_shortdiag[i], 2, 3)),   
         "B" = dat_group$fake_diag[i] <- paste0("2", substr(dat_group$fake_shortdiag[i], 2, 3)),   
         "C" = dat_group$fake_diag[i] <- paste0("3", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "D" = dat_group$fake_diag[i] <- paste0("4", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "E" = dat_group$fake_diag[i] <- paste0("5", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "F" = dat_group$fake_diag[i] <- paste0("6", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "G" = dat_group$fake_diag[i] <- paste0("7", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "H" = dat_group$fake_diag[i] <- paste0("8", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "I" = dat_group$fake_diag[i] <- paste0("9", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "J" = dat_group$fake_diag[i] <- paste0("10", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "K" = dat_group$fake_diag[i] <- paste0("11", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "L" = dat_group$fake_diag[i] <- paste0("12", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "M" = dat_group$fake_diag[i] <- paste0("13", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "N" = dat_group$fake_diag[i] <- paste0("14", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "O" = dat_group$fake_diag[i] <- paste0("15", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "P" = dat_group$fake_diag[i] <- paste0("16", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "Q" = dat_group$fake_diag[i] <- paste0("17", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "R" = dat_group$fake_diag[i] <- paste0("18", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "S" = dat_group$fake_diag[i] <- paste0("19", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "T" = dat_group$fake_diag[i] <- paste0("20", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "U" = dat_group$fake_diag[i] <- paste0("21", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "V" = dat_group$fake_diag[i] <- paste0("22", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "W" = dat_group$fake_diag[i] <- paste0("23", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "X" = dat_group$fake_diag[i] <- paste0("24", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "Y" = dat_group$fake_diag[i] <- paste0("25", substr(dat_group$fake_shortdiag[i], 2, 3)),
         "Z" = dat_group$fake_diag[i] <- paste0("26", substr(dat_group$fake_shortdiag[i], 2, 3)),
         dat_group$fake_diag[i] <- paste0("?", substr(dat_group$fake_shortdiag[i], 2, 3)) )   
}

dat_group$fake_diag <- as.integer(dat_group$fake_diag)



dat_group <- dat_group %>% 
  mutate(fake_dgroup = case_when(
    .$fake_diag < 300 ~ "1",
    .$fake_diag <= 448 ~ "2",
    ((.$fake_diag <= 489) & (.$fake_diag >= 450)) ~ "3",
    ((.$fake_diag <= 590) & (.$fake_diag >= 500)) ~ "4",
    ((.$fake_diag < 700) & (.$fake_diag >= 600)) ~ "5",
    .$fake_diag < 800 ~ "6",
    .$fake_diag <= 859 ~ "7",
    .$fake_diag <= 895 ~ "8",
    ((.$fake_diag < 1000) & (.$fake_diag >= 900)) ~ "9",
    .$fake_diag < 1100 ~ "10",
    .$fake_diag <= 1193 ~ "11",
    ((.$fake_diag < 1300) & (.$fake_diag >= 1200)) ~ "12",
    .$fake_diag < 1400 ~ "13",
    .$fake_diag < 1500 ~ "14",
    .$fake_diag < 1600 ~ "15",
    .$fake_diag <= 1696 ~ "16",
    ((.$fake_diag < 1800) & (.$fake_diag >= 1700)) ~ "17",
    .$fake_diag < 1900 ~ "18",
    .$fake_diag <= 2098 ~ "19",
    ((.$fake_diag <= 2598) & (.$fake_diag >= 2201)) ~ "20",
    ((.$fake_diag < 2700) & (.$fake_diag >= 2600)) ~ "21",
    ((.$fake_diag <= 2185) & (.$fake_diag >= 2100)) ~ "22"
  )
  ) %>% 
  mutate(fake_dgroup = factor(.$fake_dgroup, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22"), ordered = TRUE))



#dat_group$date <- substring(dat_group$fake_doa,4,10)
dat_group$fake_year <- substring(dat_group$fake_doa,7,10)
dat_group$fake_month <- substring(dat_group$fake_doa,4,5)

dat_group$fake_normage <- (dat_group$fake_age-mean(dat_group$fake_age))/sd(dat_group$fake_age)



analysis1 <- data.frame(fake_normage = dat_group$fake_normage, 
                        fake_logcis = log(dat_group$fake_cis), 
                        fake_spec = as.factor(dat_group$fake_spec), 
                        fake_sex = as.factor(dat_group$fake_sex), 
                        fake_marital = as.factor(dat_group$fake_marital), 
                        fake_sigfac = as.factor(dat_group$fake_sigfac), 
                        fake_wait = as.factor(dat_group$fake_wait), 
                        fake_year = as.numeric(dat_group$fake_year), 
                        fake_month = as.numeric(dat_group$fake_month),
                        fake_dgroup = as.factor(dat_group$fake_dgroup), 
                        fake_admission2 = as.factor(dat_group$fake_admission2))

analysis2 <- data.frame(fake_normage = dat_group$fake_normage, 
                        fake_logcis = log(dat_group$fake_cis), 
                        fake_spec = as.factor(dat_group$fake_spec), 
                        fake_sex = as.factor(dat_group$fake_sex), 
                        fake_marital = as.factor(dat_group$fake_marital), 
                        fake_ipdc = as.factor(dat_group$fake_ipdc),
                        fake_sigfac = as.factor(dat_group$fake_sigfac), 
                        fake_wait = as.factor(dat_group$fake_wait),
                        fake_year = as.numeric(dat_group$fake_year),
                        fake_month = as.numeric(dat_group$fake_month),
                        fake_dgroup = as.factor(dat_group$fake_dgroup),
                        fake_admission2 = as.factor(dat_group$fake_admission2),
                        fake_cubelos = (dat_group$fake_los)^(1/3))


n <- length(dat_group[,1])
n1 <- as.integer(n*0.8) 
n2 <- as.integer(n*0.2) + 1



sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 

train_1 <- analysis1[sampleNum,]
test_1 <- analysis1[-sampleNum,]

train_2 <- analysis2[sampleNum,]
test_2 <- analysis2[-sampleNum,]


