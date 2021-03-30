numQ = seq(4, 60, 8)
numS = c(10, 25, 100, 500)
numC = seq(2, 5, 1)
p = seq(0, 1, 0.05)
m = 10000
colfunc = colorRampPalette(c("#FBE7FF", "#736277"))
c = colfunc(length(numQ))

par(mfrow=c(2,2))

for(h in 1:length(numS)){
  for(k in 1:length(numC)){
    mat = matrix(NA, nrow=length(numQ), ncol=length(p))
    colnames(mat) = p
    rownames(mat) = paste(numQ, "Questions")
    mat_stat = matrix(NA, nrow=length(numQ), ncol=7)
    colnames(mat_stat) = c("Min.", "25%", "50%", "75%", "Max.", "Mean", "SD")
    rownames(mat_stat) = paste(numQ, "Questions")
    
    plot(0, 0, xlim=c(0, 1)*100, ylim=c(0, 1)*100, col="white",
         main=paste0(numS[h], " Students. ", numC[k], " Choices per Question"), 
         xlab = "Percentage of Students Scores up to nth Percent", ylab = "Test Score")
    abline(h=1/numC[k]*100, lty=2, lwd=2, col="#5A4A5F")
    legend("topleft", legend=paste(numQ, "Questions"), col=c, lty=1, lwd=2, cex=0.75)
    
    for(i in 1:length(numQ)){
      mc_mat = matrix(NA, nrow = m, ncol = length(p))
      mc_mat_stat = matrix(NA, nrow = m, ncol = 7)
      for(j in 1:m){
        X = rbinom(numS[h], numQ[i], 1/numC[k])
        Scores = X/numQ[i]*100
        mc_mat[j,] = quantile(Scores, probs = p)
        mc_mat_stat[j,] = c(quantile(Scores, probs = c(0, 0.25, 0.5, 0.75, 1)), mean(Scores), sd(Scores))
      }
      mat[i,] = colMeans(mc_mat)
      mat_stat[i,] = colMeans(mc_mat_stat)
      lines(p*100, mat[i,], col=c[i], lwd=2)
    }
    print(paste0(numC[k], " Choices. (", numS[h], " Students)"))
    print(mat_stat)
  }
}
