#calculate confidence interval of q(t,s)
iv <- read.csv("y(t,s).csv",header = F); # data of y(t,s)
nv <- read.csv("N(t,s).csv",header = F); # data of N(t,s)
lci <- data.frame(matrix(rep(NA,length(iv[1,])*length(iv[,1])),nrow=length(iv[,1])));
uci <- data.frame(matrix(rep(NA,length(iv[1,])*length(iv[,1])),nrow=length(iv[,1])));
for(cnt1 in 1:length(iv[,1])){
  for(cnt2 in 1:length(iv[1,])){
    if(nv[cnt1,cnt2]==0){
      lci[cnt1,cnt2] <- 0;
      uci[cnt1,cnt2] <- 0;
    }
    else{
      lci[cnt1,cnt2] <- prop.test(iv[cnt1,cnt2],nv[cnt1,cnt2])$conf.int[1]; #lower bound of q(t,s)
      uci[cnt1,cnt2] <- prop.test(iv[cnt1,cnt2],nv[cnt1,cnt2])$conf.int[2]; #upper bound of q(t,s)
    }
  }
}
