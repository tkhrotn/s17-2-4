library(readr)

FR_Yodogawa <- read_csv("data/static-data/FR_Yodogawa.csv", 
                        col_types = cols("芥川" = col_double(), 
                                         YMDS = col_date(format = "%Y/%m/%d"), 
                                         "枚方" = col_double()))
View(FR_Yodogawa)

ConCR_Yodogawa <- read_csv("data/static-data/ConCR_Yodogawa.csv", 
                           col_types = cols(YMDS = col_date(format = "%Y/%m/%d")))
View(ConCR_Yodogawa)


# %YY/%m/%d
ymd <- FR_Yodogawa$YMDS
#RIVER DISCHARGE
   Akutagawa_df <- data.frame(      "芥川", "河川流量", ymd, FR_Yodogawa$芥川)
Yoshidabashi_df <- data.frame("吉田橋上流", "河川流量", ymd, FR_Yodogawa$吉田橋上流)
    Takahama_df <- data.frame(      "高浜", "河川流量", ymd, FR_Yodogawa$高浜)
        Oobe_df <- data.frame(      "小戸", "河川流量", ymd, FR_Yodogawa$小戸)
Kamitodoromi_df <- data.frame("上止々呂美", "河川流量", ymd, FR_Yodogawa$上止々呂美)
    Hirakata_df <- data.frame(      "枚方", "河川流量", ymd, FR_Yodogawa$枚方)
      Tokura_df <- data.frame(      "利倉", "河川流量", ymd, FR_Yodogawa$利倉)
colnames(Akutagawa_df)    <- c("NAME","ITEM","YMD","VALUE")
colnames(Yoshidabashi_df) <- c("NAME","ITEM","YMD","VALUE")
colnames(Takahama_df)     <- c("NAME","ITEM","YMD","VALUE")
colnames(Oobe_df)         <- c("NAME","ITEM","YMD","VALUE")
colnames(Kamitodoromi_df) <- c("NAME","ITEM","YMD","VALUE")
colnames(Hirakata_df)     <- c("NAME","ITEM","YMD","VALUE")
colnames(Tokura_df)       <- c("NAME","ITEM","YMD","VALUE")

#CONTAMINAT CONCENTRATION
 Y1_df <- data.frame(     "Y1", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y1)
 Y2_df <- data.frame(     "Y2", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y2)
 Y3_df <- data.frame(     "Y3", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y3)
 Y4_df <- data.frame(     "Y4", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y4)
 Y5_df <- data.frame(     "Y5", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y5)
 Y6_df <- data.frame(     "Y6", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y6)
 Y7_df <- data.frame(     "Y7", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y7)
 Y8_df <- data.frame(     "Y8", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y8)
 Y9_df <- data.frame(     "Y9", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y9)
Y10_df <- data.frame(    "Y10", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y10)
Y11_df <- data.frame(    "Y11", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y11)
Y12_df <- data.frame(    "Y12", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y12)
Y13_df <- data.frame(    "Y13", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y13)
Y14_df <- data.frame(    "Y14", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y14)
Y15_df <- data.frame(    "Y15", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y15)
Y16_df <- data.frame(    "Y16", "汚染濃度", ConCR_Yodogawa$YMDS, ConCR_Yodogawa$Y16)

#paste("No","NAME","ITEM",YMD","VALUE",sep = ",")
for(i in 1:nrow(FR_Yodogawa)){
  write.table(   Akutagawa_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(Yoshidabashi_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(    Takahama_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(        Oobe_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(Kamitodoromi_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(    Hirakata_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(      Tokura_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y1_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y2_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y3_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y4_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y5_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y6_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y7_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y8_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(          Y9_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(         Y10_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(         Y11_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(         Y12_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(         Y13_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(         Y14_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(         Y15_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  write.table(         Y16_df[i,1:4],file="data/static-data/YDR_basin2019.csv",sep = ",",append=TRUE,col.names=FALSE, fileEncoding = "UTF-8")
  }

