data1 <- read.csv("C:/Users/hyolim/Desktop/林力盒籍/data_2018_cat.csv")

a <- data1$careAddr
a1 <- strsplit(a, split= " ") ; a1

df1 <- data.frame("careAddr_do"=c(NA, NA), "careAddr_si"=c(NA,NA), "careAddr_ub"=c(NA, NA)) ; df1

for (i in 1:length(a1)){
  df1[i,] <- a1[[i]][1:3]
}
df1

df1$careAddr_dosi = NA

for (i in 1:nrow(df1)){
  df1$careAddr_dosi[i] <- paste(df1[i,1],df1[i,2])
}
df1

# 2. orgNm 傈贸府
b <- data1$orgNm ; b
b1 <- strsplit(b, split= " ") ; b1
df11 <- data.frame("orgNm_do"=c(NA, NA), "orgNm_si"=c(NA,NA), "orgNm_ub"=c(NA, NA)) ; df11

for (i in 1:length(b1)){
  df11[i,] <- b1[[i]][1:3]
}
df11

df11$orgNm_dosi = NA

for (i in 1:nrow(df11)){
  if(!is.na(df11[i,2]) == T){
    df11$orgNm_dosi[i] <- paste(df11[i,1],df11[i,2])
  }
}
df11
