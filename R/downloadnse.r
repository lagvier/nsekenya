
#' downloadnse function

#' The function retrieves the archived data, downloads new data and combine both of the datasets. Then archive the updated version of the data

#' @keywords nse kenya, stock market, kenya bourse
#' @export
#' downloadnse (havingIP)

downloadnse <- function(){

  havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}


token <- rdrop2::drop_auth()

saveRDS(token, "droptoken.rds")


  if(!havingIP()){
        stop( cat("You are not connected to the internet!\n
          Check you internet connection and try again...!\n"))
      }else{
        Today<-Sys.Date();


        A <- rdrop2::drop_read_csv("/Personal projects/COMPLETE NSE DATA.csv",header=TRUE)
        if(dim(A)[1]>0){
             Start<-max(as.Date(strptime(A$DATE,format="%Y-%m-%d")));
        }else{
             Start<-Today-30;
        }
          nse<-"http://live.mystocks.co.ke/price_list"
          Header<-c("CODE", "NAME", "YRLow", "YRHigh", "Low", "High", "Close", "Open",
                      "Change1", "Change2","Volume")
          dates<-Start
          k<-(Today+1)-Start
                sa<-as.Date(strptime('2014-02-14',format="%Y-%m-%d"))
                    sa<-format(sa-1,"/%Y%m%d")
                    n<-paste("http://live.mystocks.co.ke/price_list",sa,sep='');
                    n1<-XML::readHTMLTable(n,header=F,as.data.frame = TRUE,isURL=T,trim=T,
                            stringsAsFactors = FALSE,skip.rows=c(1,2,3))
                    n2<-n1$pricelist
                    n2<-n2[!is.na(n2$V2),]
                    n2<-n2[!is.na(n2$V2),]
                    n2$V11<-NULL
                    n2$V13<-NULL
                    names(n2)<-Header
                    DATE<-format(Today,"%m/%d/%Y")
                    nseData<-cbind(n2,DATE)
                    N<-length(nseData[,1])
                    nseData<-nseData[-(1:N),]
                    nseData2<-nseData

          for(i in 1:k){
            day<-format(dates,"%a")
            dat<-format(dates,"%d%m")
            add<-format(dates,"/%Y%m%d")
            DATE<-format(dates,"%m/%d/%Y")
            add<-as.character(add)
            add<-paste(nse,add,sep="")
              if(RCurl::url.exists(add)){
                  nse1<-XML::readHTMLTable(add,header=F,as.data.frame = TRUE,isURL=T,trim=T,
                            stringsAsFactors = FALSE,skip.rows=c(1,2,3))
                  nse1<-nse1$pricelist
                  check4<-length(nse1)
                  if(check4!=0){
                      nse1$V11<-NULL
                      nse1$V13<-NULL
                      names(nse1)<-Header
                      nseData1<-cbind(nse1,DATE)
                      message1<-':       Data available';
              nseData<-rbind(nseData,nseData1)
            }else{
              message1<-':       No data';
            }
          }else{
              message1<-':       No data';
            }
            dates<-dates+1
            cat(add,message1,'\n')
          }
        }

        B<-subset(nseData,select=c("CODE","NAME","YRLow","YRHigh","Low","High","Close","Open","Volume","DATE"))
           B$DATE<-as.Date(strptime(B$DATE,format="%m/%d/%Y"));
           B<-B[which(B$NAME!='NA' & B$NAME!=""),];
           B$YRLow<-ifelse(B$YRLow=='-','NA',B$YRLow); B$YRLow<-as.numeric(B$YRLow);
           B$YRHigh<-ifelse(B$YRHigh=='-','NA',B$YRHigh);B$YRHigh<-as.numeric(B$YRHigh);
           B$Low<-ifelse(B$Low=='-','NA',B$Low); B$Low<-as.numeric(B$Low);
           B$High<-ifelse(B$High=='-','NA',B$High); B$High<-as.numeric(B$High);
           B$Close<-ifelse(B$Close=='-','NA',B$Close); B$Close<-as.numeric(B$Close);
           B$Open<-ifelse(B$Open=='-','NA',B$Open); B$Open<-as.numeric(B$Open);
           B$Volume<-ifelse(B$Volume=='-','NA',B$Volume);
            B$Volume<-gsub("\\,", "", B$Volume);
            B$Volume<-ifelse(tolower(substr(B$Volume,nchar(B$Volume),nchar(B$Volume)))=='m',
                      as.numeric(substr(B$Volume,1,nchar(B$Volume)-1))*1000000,
                  as.integer(B$Volume))
            B$CODE<-as.factor(B$CODE);
            B$NAME<-as.factor(B$NAME);
            A$DATE<-as.Date(as.character(A$DATE,format="%Y-%m-%d"));
            B<-rbind(A,B);
            B<-B[order(B$CODE,B$DATE,decreasing = T),]
      write.csv(B,file="COMPLETE NSE DATA.csv",row.names=F)
      rdrop2::drop_upload(file="COMPLETE NSE DATA.csv", dest = "/Personal projects", overwrite = TRUE)
      file.remove("COMPLETE NSE DATA.csv", showWarnings = TRUE)
      file.remove("droptoken.rds", showWarnings = FALSE)
}
