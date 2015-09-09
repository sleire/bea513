
FwdCurve <- function(price,start,end,prior=trig){
  # Creates a smooth forward curve from futures contract prices
  #
  # Args:
  # Price vector
  # Start date vector
  # End date vector
  # Prior function
  #
  # Returns:
  # Data frame with hourly forward prices
  
  # basic input control
  stopifnot(length(price)==length(start), length(price)==length(end))
  
  # dates and non-overlapping period list
  start<-as.POSIXct(start,tz = "GMT")
  end<-as.POSIXct(end,tz = "GMT")+60*60*24
  #start_adj<-start[!start==end]           # remove duplicates in start
  #end_adj<-end[!end==start]               # remove duplicates in end
  #adj<-as.Date(setdiff(end_adj+1,start_adj),origin="1970-01-01")
  #list<-c(start,adj)                      # merge start and adjusted list
  #nduplist<-sort(list[!duplicated(list)]) # non-duplicated date list
  nduplist<-sort(c(start,end)[!duplicated(c(start,end))])
  
  # dates, day-of-year number sequence and knots
  Date<-seq(min(start),max(end),by="hour")
  hours<-as.numeric(strftime(Date,format="%H"))*as.numeric(strftime(Date,format="%H"))
  k<-match(nduplist,Date)
  
  # select prior function
  if (prior==trigtrend)
  {
    prior<-35*exp(0.03/365*cumsum(hours/hours))+2.437*sin(2*hours*pi/365)+4.366*cos(2*hours*pi/365)
  } 
  else if (prior==trig)
  {
    prior<-35+2.437*sin(2*hours*pi/365)+4.366*cos(2*hours*pi/365)
  } 
  else
    prior<-rep(0,length(hours))
  
  # build (5nx5n) matrix H
  d<-as.numeric(diff(nduplist))           # knot deltas (spline lengths)
  n<-length(d)                            # number of knot deltas
  H<-matrix(0,nrow=5*n,ncol=5*n)
  index<-1                                # create n hj matrices
  for (j in 1:n)
  {                                       # insert hj's at h(5j-4,5j-4) in H
    hj<-matrix(c(28.8*d[j]**5,18*d[j]**4,8*d[j]**3,0,0,18*d[j]**4,
                 12*d[j]**3,6*d[j]**2,0,0,8*d[j]**3,6*d[j]**2,
                 4*d[j],0,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5)
    H[index:(index+5-1),index:(index+5-1)]<-hj
    index<-index+5
  }
  
  # build (3n+m-2)x5n matrix A in 4 steps
  
  # step 1/4: K 3(n-1)x5n matrix with knot constraints
  K<-matrix(0,nrow=3*(n-1),ncol=5*n)
  kn<-cumsum(d)                           # knots
  index1<-1                               
  index2<-1                               # create n-1 knj matrices
  for (j in 1:(n-1))
  {                                       # insert knj's at k(3j-2,5j-4) in K
    knj<-matrix(c(-kn[j]**4,-4*kn[j]**3,-12*kn[j]**2,-kn[j]**3,-3*kn[j]**2,
                  -6*kn[j],-kn[j]**2,-2*kn[j],-2,-kn[j],-1,0,-1,0,0,kn[j]**4,
                  4*kn[j]**3,12*kn[j]**2,kn[j]**3,3*kn[j]**2,6*kn[j],
                  d[j]**2,2*d[j],2,d[j],1,0,1,0,0),nrow=3,ncol=10)
    K[index1:(index1+3-1),index2:(index2+10-1)]<-knj
    index1<-index1+3
    index2<-index2+5
  }
  
  # step 2/4: (5n) vector E with end constraint
  E<-c(rep(0,5*(n-1)),4*kn[n]**3,3*kn[n]**2,2*kn[n],1,0)
  
  # step 3/4: (mx5n) matrix F with m price constraints (må fikses)
  md<-as.numeric(end-start+1)             # contract lengths
  m<-length(md)                           # m contracts included  
  dat<-list()                             # m start-end intervals with possible sub intervals
  int<-list()
  for (j in 1:m)
  {
    dat[[j]]<-c(start[j],nduplist[start[j]<nduplist&nduplist<end[j]],end[j])
    int[[j]]<-as.numeric(diff(c(start[j],nduplist[start[j]<nduplist&nduplist<end[j]],end[j])))+1
  }
  
  
  f<-matrix(c(1/5*md**5,1/4*md**4,1/3*md**3,1/2*md**2,md),
            nrow=length(md),ncol=5)
  
  F<-matrix(0,nrow=m,ncol=5*n)
  for (j in 1:m)
  {
    for (i in 1:n)
    {
      F[j,]<-c(rep(0,(5*(i-1))),f[j,],rep(0,(5*(n-i)-5)))
    }
  }
  
  # step 4/4: merge K, E and F to create A
  # A<-rbind(K,E,F)
  
  # build (3n+m-2) vector b (må fikses)
  # b<-c(rep(0,3*(n-1)),0,price)
  
  # solve equations with Lagrange: x'Hx + a'(Ax-b)
  # |2H A'| |x| = |0|
  # |A  0 | |a|   |b|  
  # L<-rbind(cbind(2*H,t(A)),cbind(A,matrix(0,5*n,5*n)))
  # R<-c(rep(0,5*n),b)
  # S<-solve(L,R)
  
  
  # forward curve vector
  FwdCurve<-20
  
  
  # data frame with results
  return(data.frame(Date,FwdCurve))
}