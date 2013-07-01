##############################################################################
### Conventional Top-down Forecast based on Method "F" from Gross and Sohl ###
##############################################################################

td.gsF <- function(data,h,p.f.mat,c.sum,scoef.l) 
{ 
  # Method "F" from Gross and Sohl implemented here
  level.ave <- split(matrix(colMeans(allts(data)),nrow=1),rep(1:length(data$m),data$m))
  tdm2.l <- list(matrix(p.f.mat[,1],ncol=1))
  if(length(data$m)>1)
  {
    for (j in 2:length(data$m))
    {
      tmp.mat <- matrix(NA,h,data$m[[j]])
      for (i in 1:(length(c.sum[[j-1]])-1))
        tmp.mat[,(c.sum[[j-1]][i]+1):c.sum[[j-1]][i+1]] <- tdm2.l[[j-1]][,i]
      ppl.con <- matrix(level.ave[[j]]/(level.ave[[j]]%*%scoef.l[[j-1]]),h,data$m[[j]],byrow=TRUE)
      tdm2.l[[j]] <- tmp.mat * ppl.con
    }
  }
  tdm2.l <- ts(tdm2.l[[length(data$m)]],start=tsp(data$y)[2]+1/frequency(data$y),frequency=frequency(data$y))
  colnames(tdm2.l) <- colnames(data$y)
  data$oldy <- data$y
  data$y <- tdm2.l
  return(data)
} 



##############################################################################
### Conventional Top-down Forecast based on Method "A" from Gross and Sohl ###
##############################################################################

td.gsA <- function (data, h, p.f.mat, c.sum, index, scoef.l) 
{
  # Method "A" from Gross and Sohl implemented here
  # Construct in-sample proportion matrix
  fulldata <- allts(data)
  insamp.porps <- matrix(NA, nrow = nrow(fulldata), ncol = ncol(fulldata))
  for (i in 1:nrow(insamp.porps)) 
  {
    insamp.porps[i, 1] <- 1
    if (length(data$m) > 1) 
    {
      for (j in 2:length(data$m)) 
      {
        insamp.porps[i, ((index[j-1] + 1):(index[j]))] <- 
          split(fulldata[i,], rep(1:length(data$m), data$m))[[j]]/
          split(fulldata[i,], rep(1:length(data$m), data$m))[[j]] %*% scoef.l[[j-1]]
      }
    }
  }
  propmat <- matrix(rep(colMeans(insamp.porps), h), h, byrow = TRUE)
  tdmA.l <- list(matrix(p.f.mat[, 1], ncol = 1))
  if (length(data$m) > 1) 
  {
    for (j in 2:length(data$m)) 
    {
      tmp.mat <- matrix(NA, h, data$m[[j]])
      for (i in 1:(length(c.sum[[j-1]]) - 1)) 
        tmp.mat[, (c.sum[[j-1]][i] + 1):c.sum[[j-1]][i+1]] <- tdmA.l[[j-1]][,i]
      ppl.conA <- propmat[, ((index[j-1] + 1):(index[j]))]
      tdmA.l[[j]] <- tmp.mat * ppl.conA
    }
  }
  tdmA.l <- ts(tdmA.l[[length(data$m)]], start = tsp(data$y)[2] + 
      1/frequency(data$y), frequency = frequency(data$y))
  colnames(tdmA.l) <- colnames(data$y)
  data$oldy <- data$y
  data$y <- tdmA.l

  # data$tmp.mat <- tmp.mat
  # data$ppl.conA <- ppl.conA
  # data$tdmA.l <- tdmA.l
  # data$insampleprop <- colMeans(insamp.porps)

  return(data)
}

#################################################################
### Top-down forecast by Roman's Forecast Proportion Approach ###
#################################################################


td.rFP <- function (data, h, p.f.mat, c.sum, scoef.l, index) 
{
  # For calculating proportion we need the point forecast matrix at each level (p.f.mat.l)
  # and the proportion per level (ppl) of the hierarchy
  # and the top-down matrix per level (tdm.l) of the hierarchy
  tdm.l <- list(matrix(p.f.mat[, index[1]], h, 1))
  if (length(data$m) > 1) 
  {
    for (j in 2:length(data$m)) 
    {
      tmp.mat <- matrix(NA, h, data$m[j])
      for (i in 1:ncol(tdm.l[[j-1]])) 
        tmp.mat[, (c.sum[[j-1]][i] + 1):(c.sum[[j-1]][i+1])] <- tdm.l[[j-1]][,i]
      p.f.mat.l <- p.f.mat[, (index[j-1] + 1):index[j]]
      ppl.gs <- matrix(p.f.mat.l,nrow=dim(p.f.mat)[1])/(matrix(p.f.mat.l,nrow=dim(p.f.mat)[1]) %*% scoef.l[[j-1]])
      tdm.l[[j]] <- tmp.mat * ppl.gs
    }
  }
  tdm.l <- ts(tdm.l[[length(data$m)]], start = tsp(data$y)[2] + 
      1/frequency(data$y), frequency = frequency(data$y))
  tdm.l[is.na(tdm.l)] <- 0
  colnames(tdm.l) <- colnames(data$y)
  data$oldy <- data$y
  data$y <- tdm.l
  return(data)
}

