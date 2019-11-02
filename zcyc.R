yields = function(Initial_Date,Final_Date){
# Packages
 packages = c("rvest","httr","functional","xts")
 new.packages = packages[!(packages %in% installed.packages()[,"Package"])]
 if(length(new.packages)) install.packages(new.packages)
 suppressMessages(library(rvest))
 suppressMessages(library(httr))
 suppressMessages(library(functional))
 suppressMessages(library(xts))
dates = format(seq(as.Date(Initial_Date), as.Date(Final_Date), 'day'), format="%d/%m/%Y", tz="UTC")
mat = matrix(NA,length(dates),(length(Maturities)+1))
# Scraping
for(i in 1:length(dates)){
 di = GET("https://www.cbr.ru/eng/hd_base/zcyc_params/",query=list(DateTo=dates[i]))
 data = read_html(di) %>% html_nodes("table") %>% html_nodes("td") %>% html_text()
 if(data[1]==dates[i]){
  mat[i,] = data[1:13]
  #data.frame(matrix(data[1:13], ncol=13, byrow=TRUE))
 }else{
    i=i
 }
 pb = txtProgressBar(min = (1/length(dates)), max = length(dates), style = 3)
 setTxtProgressBar(pb,i)
}
mat = na.omit(mat)
mat = xts(matrix(as.numeric(mat[,2:13]),dim(mat)[1],(dim(mat)[2]-1)),order.by=as.Date(mat[,1], format="%d/%m/%Y", tz="UTC"))
Maturities = c(3,6,9,12,24,36,60,84,120,180,240,360)
colnames(mat) = paste0("M",Maturities)
return(mat)
}

## Example
Initial_Date = "2019/10/01" "YYYY/MM/DD"
Final_Date = "2019/11/01"

yieldsRU = yields(Initial_Date=Initial_Date,Final_Date=Final_Date)
