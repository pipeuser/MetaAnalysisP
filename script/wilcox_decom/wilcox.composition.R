args <- commandArgs(T)
print("P-values tested for compositionality")
print("need to confirm the name of pro same")

if(length(args) != 4){
	stop("Rscript mantel.R [profile]  [group]  [paired] [prefix]
	 	pro1: matrix/profile
		group : group
	 	paired :  F/T
		prefix : out")
}

# load data
pro1 <- read.table(args[1], header=T, row.names=1, check.names=F, sep="\t")
group <- read.table(args[2], header=T, row.names=1, check.names=F, sep="\t")
paired <- args[3]
prefix <- args[4]

# check pro1 and group's sample name
name <- intersect(colnames(pro1), rownames(group))
pro1 <- t(pro1[, name])
group <- group[name, , drop = F]

# function for statistical test 
wilcox = function (M, skip) {
	# We remove the unknown term
	# We convert the matrix to a dataframe
	M <- as.data.frame(M)
	# We add to the matrix the grouping about time points and the pairing samples info
	group$Group <- as.factor(group[,1])
	#print(group$Group)
	M.strat<-cbind(M, Group = group$Group)
	# We perform the wilcoxon signed-rank tests
	g1.g2.pvals <- c()
	num.taxa <- dim(M.strat)[2]-1
	for (i in 1:num.taxa){
		#print(i)
		#print(M.strat$Group)
		#print(M.strat[,i])
		if(paired == "T"){
  			g1.g2.pvals[i] = wilcox.test(M.strat[,i] ~ Group, M.strat, paired = TRUE)$p.value
		}else{
			g1.g2.pvals[i] = wilcox.test(M.strat[,i] ~ Group, M.strat)$p.value
		}
	}
# we assign names to the pvalues
	names(g1.g2.pvals) <- colnames(M)
    newrow<-c(skip, NA, NA)
# We create a table with the taxa and the Pvalues for the test
	table<-data.frame(
  		Taxa = as.character(names(g1.g2.pvals)),
  		Pvalue = as.numeric(g1.g2.pvals),
  		stringsAsFactors=FALSE)
	
	table <- rbind(table, newrow)
	return(table)
} 

	
# main script
Pvalue.g1.g2 <- data.frame(matrix(ncol = dim(pro1)[2], nrow = dim(pro1)[2]))
# The for loop will go through all the species in the species table leaving this species out and running a Wilcoxon test for each of the rest to know if there are significant differences on the relativs abundance of these species between the 2 groups when another of the species fully disappears
for (i in 1:length(colnames(pro1))) {
  #print(i)
  #print(colnames(pro1)[i])
  # Leaving one out
  M.one.out <- pro1[,-i]
  
  # Renormalizing
  M.one.out.norm <- M.one.out/rowSums(M.one.out)
  
  # Running Wilcoxon test
  compos.test <- wilcox(M.one.out.norm, colnames(pro1)[i])
  #print(compos.test)
  compos.test <- compos.test[order(compos.test[,1]),]
  Pvalue.g1.g2[,i] <- compos.test$Pvalue
}
# Adding the right names
colnames(Pvalue.g1.g2) <- colnames(pro1)
rownames(Pvalue.g1.g2) <- sort(colnames(pro1))

pval.test.g1.g2 <- data.frame(sapply(Pvalue.g1.g2[,1:dim(Pvalue.g1.g2)[2]], as.numeric))
rownames(pval.test.g1.g2) <- rownames(Pvalue.g1.g2)
pval.test.g1.g2$Least.signif.pval <- apply(pval.test.g1.g2[,1:dim(pval.test.g1.g2)[2]],1,max, na.rm=TRUE)
# We create a table to study the results after the compositionality test
P.value.after.compositionality.test <- data.frame(Taxa = rownames(pval.test.g1.g2), Pvalue =  pval.test.g1.g2$Least.signif.pval)
# Sorting
P.value.after.compositionality.test <- P.value.after.compositionality.test[with(P.value.after.compositionality.test, order(Pvalue)),]
# Writing a table with the results
write.table(P.value.after.compositionality.test, 
	file= paste0(prefix, "wilcox.compositionality.tab"), quote=FALSE, sep='\t', row.names=FALSE, col.names=TRUE)




