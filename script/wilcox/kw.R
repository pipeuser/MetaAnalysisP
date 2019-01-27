# note: cofig order must the same pr
# debug if the pr have the NA( the rank function don't perform the NA value default )
# add the direction used the median value(if the profile have the overmuch 0, suggest use the mean rank),we can use the occrance as the weight
args <- commandArgs(T)

if(length(args)!=4){
  stop("not enough arguments :n=4")
}

#read files
pr <- read.table(args[1],check.names=F,header=T,row.names=1,sep="\t")  #the colnames will change if no check.names
config <- read.table(args[2],header=T,row.names=1,check.names=F)
alpha <- as.numeric(args[3])  # 0.05
out.dir <- args[4]
#match names of files
rownames(pr)[rownames(pr)=="unclassed"] <- "Unclassified"
inter <- intersect(rownames(config),colnames(pr))
pr <-pr[,inter]
config <-config[inter,,drop=F]
pr <- pr[rowSums(pr)>0,]

sum_name <- sum(ifelse(rownames(config)==colnames(pr),1,0))
if(sum_name==nrow(config)){print ("All sample matched")}
#variables
num <- nrow(pr)
fr <- as.factor(config[, 1])
group <- levels(fr)
#output
len <- length(group)
num2 <- len*len/2+3.5*len + 2
out <- matrix(NA, num, num2)
#kurskal test
ktp <- apply(pr, 1, function(x){kruskal.test(x ~ fr)$p.value})
#post hoc dunn test
library(PMCMR)
for (i in 1:num) {
  print (i)
  index <- is.na(fr)
  fr1 <- fr[!index]
  rk  <- rank(pr[i,][!index])
  pr1 <- pr[i,][!index]
  pr1 <- as.numeric(pr1)
  res <- c(ktp[i], tapply(pr1, fr1, median),tapply(pr1,fr1,mean), tapply(pr1>0, fr1,mean))
  dtp <- posthoc.kruskal.dunn.test(pr1, fr1, p.adjust.method = "BH")$p.value
  dtp <- cbind(dtp, rep(NA, len - 1))
  dtp <- rbind(rep(NA, len), dtp)
  dtp[upper.tri(dtp)] <- t(dtp)[upper.tri(dtp)]
  rownames(dtp)[1] <- colnames(dtp)[1]
  colnames(dtp)[len] <- rownames(dtp)[len]

  mean_rank <- tapply(rank(pr1),fr1,mean)
        res <- c(res,dtp[lower.tri(dtp)], mean_rank)
    #
        conclude <- rep(0,2*len-1)
        or <- order(mean_rank)
        conclude[2*(1:len)-1] <- group[or]
        op <- rep(1,len-1)
        for(j in 1:(len-1)){op[j] <- dtp[or[j],or[j+1]]}
        symbol <- rep("=",len-1)
        symbol[!is.na(op) & op <= alpha] <- "<"
        symbol[is.na(op) | op == 1] <- "<=>"
        for(x in 1:(len-1)){
          if(symbol[x]=="<"){
            p_tmp <- c()
            for(y in 1:x){
              for(z in (x+1):len){
                p_tmp <- c(p_tmp,dtp[or[y],or[z]])
              }
            }
            if(any(p_tmp>0.05)){symbol[x] <- "="}
          }
        }

        conclude[(1:(len - 1)) * 2] <- symbol
  res <- c(res, paste(conclude, collapse = " "))
  if(length(res)==ncol(out)){out[i, ] <- res}else{print (res)}
}
rownames(out) <- row.names(pr)
cn <- c("kw.p", paste("median", group[1:len], sep = "_"))
cn <- c(cn,paste("mean", group[1:len], sep = "_"))
cn <- c(cn, paste("or", group[1:len], sep = "_"))
cn <- c(cn, paste0("p", "_", group[row(dtp)[lower.tri(dtp)]], "_", group[col(dtp)[lower.tri(dtp)]]))
cn <- c(cn, paste("mean_rank",group[1:len], sep = "_"))
cn <- c(cn, "nearby")
colnames(out) <- cn

write.table(out, out.dir, col.names = NA, sep = "\t", quote = F)
