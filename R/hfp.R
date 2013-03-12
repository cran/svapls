hfp <-
function (obj,gen,ind,Y){   
    genes <- Y[,1]
    num <- length(gen)
    gen.index <- rep(0,num)
    for (i in 1:num) gen.index[i] <- which(genes == gen[i]) 
    W_imp <- obj$PLS.imp
    if (max(W_imp) != 0) heatmap(t(W_imp[gen.index,ind]),Rowv = NA,Colv = NA,labRow=as.character(ind),labCol=gen,xlab="Specified subjects")
    if (max(W_imp) == 0) print("No trace of hidden subject-specific variation is found in the data")
}
