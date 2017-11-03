ReadSpan <- function (path,market="ALL"){
  # Reads SPAN® (Standard Portfolio Analysis of Risk) files from Nasdaq Commodities
  # available at ftp://ftp.nordic.nasdaqomxtrader.com/Commodities/PROD/Common/
  #
  # Args:
  # path    -   path to file
  # market  -   which market to extract contract info from:
  #   ALL   -   all contracts in file
  #   ENO   -   Electricity Nordic
  #   EDE   -   Electricity Germany
  #   ENL   -   Electricity Netherlands
  #   EUK   -   Electricity UK
  #
  # Returns:
  # Data frame with: 
  #   ContractId
  #   Ticker
  #   Start date
  #   End date
  #   Volume
  #   Close
  #   RiskInt
  #
  stopifnot(market %in% c("ALL","ENO","EDE","ENL","EUK"))
  
  ################################### Settings ################################
  
  # "offset" and "length" from Nasdaq's description of SPAN file available at
  # http://www.nasdaqomx.com/digitalAssets/91/91765_description-span-file-20131209.pdf
  # necessary for reading correct parts of the text strings
  # update code if/ when file definition is changed: c("offset","length")
  
  # 00 - File header
  ext<-c(28,5)          # Extreme value percentage position
  
  # 02 - Contract information
  cid<-c(1,12)          # Contract ID position
  tic<-c(13,24)         # Ticker position
  sta<-c(48,8)          # Start date position
  end<-c(56,8)          # End date position
  vol<-c(72,8)          # Contract volume positon
  clo<-c(80,13)         # Closing price position
  
  # 03 - Risk arrays (rid = Contract ID, r* = risk scenario nr *)
  rid<-c(1,12);          r09<-c(115,9)
  r01<-c(43,9);          r10<-c(124,9)
  r02<-c(52,9);          r11<-c(133,9)
  r03<-c(61,9);          r12<-c(142,9)
  r04<-c(70,9);          r13<-c(151,9)
  r05<-c(79,9);          r14<-c(160,9)
  r06<-c(88,9);          r15<-c(169,9)
  r07<-c(97,9);          r16<-c(178,9)
  r08<-c(106,9);         dec<-c(197,1)  # dec = position of nr of 
                                        # decimals in risk arrays
  
  ############################### End Settings ################################
  
  # read lines in SPAN file
  span<-readLines(path)
  
  # trim file header line
  he<-substring(span[substr(span,1,4)=="0001"],7)
  
  # build contract lines
  c1<-substring(span[substr(span,1,4)=="0201"],7)
  c2<-substring(span[substr(span,1,4)=="0202"],7)
  c3<-substring(span[substr(span,1,4)=="0203"],7)
  cline<-paste(paste(c1,c2,sep=""),c3,sep="")
  
  # build risk lines
  r1<-substring(span[substr(span,1,4)=="0301"],7)
  r2<-substring(span[substr(span,1,4)=="0302"],7)
  r3<-substring(span[substr(span,1,4)=="0303"],7)
  rline<-paste(paste(r1,r2,sep=""),r3,sep="")
  
  # read contract info from contract lines and store in contract data frame C
  ContractID<-substr(cline,cid[1],cid[2]-1)
  
  Ticker<-sub("\\s+$", "",substr(cline,tic[1],tic[1]+tic[2]-1))
  
  Start<-as.Date(paste(substr(substr(cline,sta[1],sta[1]+sta[2]-1),1,4),
                       substr(substr(cline,sta[1],sta[1]+sta[2]-1),5,6),
                       substr(substr(cline,sta[1],sta[1]+sta[2]-1),7,8),sep="-"))
  
  End<-as.Date(paste(substr(substr(cline,end[1],end[1]+end[2]-1),1,4),
                     substr(substr(cline,end[1],end[1]+end[2]-1),5,6),
                     substr(substr(cline,end[1],end[1]+end[2]-1),7,8),sep="-")) 
  
  Volume<-as.numeric(substr(cline,vol[1],vol[1]+vol[2]-1))
  
  Close<-as.numeric(paste(substring(substr(cline,clo[1],clo[1]+clo[2]-1),1,9),
                          substring(substr(cline,clo[1],clo[1]+clo[2]-1),10),sep="."))
  
  # contract data frame
  C<-data.frame(ContractID,Ticker,Start,End,Volume,Close)
  
  # read extreme multiplier from file header line 
  extreme<-as.numeric(paste(substring(he,ext[1],ext[1]),
                            substring(he,ext[1]+1,ext[1]+ext[2]-1),sep="."))
  
  # read risk array info from risk lines and store in risk array data frame RA
  ContractID<-substr(rline,rid[1],rid[2]-1)
  
  D<-as.numeric(substring(rline,dec[1],dec[1])) # nr of decimals i Ra  
  
  Ra01<-as.numeric(paste(substring(substr(rline,r01[1],r01[1]+r01[2]-1),1,9-D),
                         substring(substr(rline,r01[1],r01[1]+r01[2]-1),10-D),sep="."))
  
  Ra02<-as.numeric(paste(substring(substr(rline,r02[1],r02[1]+r02[2]-1),1,9-D),
                         substring(substr(rline,r02[1],r02[1]+r02[2]-1),10-D),sep="."))
  
  Ra03<-as.numeric(paste(substring(substr(rline,r03[1],r03[1]+r03[2]-1),1,9-D),
                         substring(substr(rline,r03[1],r03[1]+r03[2]-1),10-D),sep="."))
  
  Ra04<-as.numeric(paste(substring(substr(rline,r04[1],r04[1]+r04[2]-1),1,9-D),
                         substring(substr(rline,r04[1],r04[1]+r04[2]-1),10-D),sep="."))
  
  Ra05<-as.numeric(paste(substring(substr(rline,r05[1],r05[1]+r05[2]-1),1,9-D),
                         substring(substr(rline,r05[1],r05[1]+r05[2]-1),10-D),sep="."))
  
  Ra06<-as.numeric(paste(substring(substr(rline,r06[1],r06[1]+r06[2]-1),1,9-D),
                         substring(substr(rline,r06[1],r06[1]+r06[2]-1),10-D),sep="."))
  
  Ra07<-as.numeric(paste(substring(substr(rline,r07[1],r07[1]+r07[2]-1),1,9-D),
                         substring(substr(rline,r07[1],r07[1]+r07[2]-1),10-D),sep="."))
  
  Ra08<-as.numeric(paste(substring(substr(rline,r08[1],r08[1]+r08[2]-1),1,9-D),
                         substring(substr(rline,r08[1],r08[1]+r08[2]-1),10-D),sep="."))
  
  Ra09<-as.numeric(paste(substring(substr(rline,r09[1],r09[1]+r09[2]-1),1,9-D),
                         substring(substr(rline,r09[1],r09[1]+r09[2]-1),10-D),sep="."))
  
  Ra10<-as.numeric(paste(substring(substr(rline,r10[1],r10[1]+r10[2]-1),1,9-D),
                         substring(substr(rline,r10[1],r10[1]+r10[2]-1),10-D),sep="."))
  
  Ra11<-as.numeric(paste(substring(substr(rline,r11[1],r11[1]+r11[2]-1),1,9-D),
                         substring(substr(rline,r11[1],r11[1]+r11[2]-1),10-D),sep="."))
  
  Ra12<-as.numeric(paste(substring(substr(rline,r12[1],r12[1]+r12[2]-1),1,9-D),
                         substring(substr(rline,r12[1],r12[1]+r12[2]-1),10-D),sep="."))
  
  Ra13<-as.numeric(paste(substring(substr(rline,r13[1],r13[1]+r13[2]-1),1,9-D),
                         substring(substr(rline,r13[1],r13[1]+r13[2]-1),10-D),sep="."))
  
  Ra14<-as.numeric(paste(substring(substr(rline,r14[1],r14[1]+r14[2]-1),1,9-D),
                         substring(substr(rline,r14[1],r14[1]+r14[2]-1),10-D),sep="."))
  
  Ra15<-as.numeric(paste(substring(substr(rline,r15[1],r15[1]+r15[2]-1),1,9-D),
                         substring(substr(rline,r15[1],r15[1]+r15[2]-1),10-D),
                         sep="."))*extreme
  
  Ra16<-as.numeric(paste(substring(substr(rline,r16[1],r16[1]+r16[2]-1),1,9-D),
                         substring(substr(rline,r16[1],r16[1]+r16[2]-1),10-D),
                         sep="."))*extreme
  
  # risk array data frame
  RA<-data.frame(Ra01,Ra02,Ra03,Ra04,Ra05,Ra06,Ra07,Ra08,Ra09,Ra10,
                 Ra11,Ra12,Ra13,Ra14,Ra15,Ra16)
  
  # select risk interval as worst case (min of negative numbers) scenario 1-16
  RiskInt<-apply(RA,1,min)
  
  # risk interval data frame
  R<-data.frame(ContractID,RiskInt)
  
  # merge contract data frame C with risk interval data frame R
  report<-merge(C,R,by.x="ContractID",by.y="ContractID")
  
  # report selection based on user input ("market")
  if (market=="ENO"){
    selected_report<- report[grep("ENO",substr(report$Ticker,1,3)),]
    row.names(selected_report) <- NULL    # clean up
  } else if (market=="EDE"){
    selected_report<- report[grep("EDE",substr(report$Ticker,1,3)),]
    row.names(selected_report) <- NULL    # clean up
  } else if (market=="ENL"){
    selected_report<- report[grep("ENL",substr(report$Ticker,1,3)),]
    row.names(selected_report) <- NULL    # clean up
  } else if (market=="EUK"){
    selected_report<- report[grep("EUK",substr(report$Ticker,1,3)),]
    row.names(selected_report) <- NULL    # clean up
  } else {
    selected_report<-report
  }
  
  return(selected_report)
}

# Example with path to file on Nasdaq's ftp server
# ftp<-"ftp://ftp.nordic.nasdaqomxtrader.com/Commodities/PROD/Common/"
# file<-"140926/NSPANPAR_-001__-SE-_____-140926-001.txt"
# path<-paste(ftp,file,sep="")
# span.140926 <- ReadSpan(path,market="ALL")

# SPAN® is a registered trademark of Chicago Mercantile Exchange Inc.
# http://www.cmegroup.com/clearing/span-methodology.html