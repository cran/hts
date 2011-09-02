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
      ppl.con <- matrix(level.ave[[j]]/(level.ave[[j]]%*%scoef.l[[j-1]]),h,data$m[[j]],byrow=T)
      tdm2.l[[j]] <- tmp.mat * ppl.con
    }
  }
  tdm2.l <- ts(tdm2.l[[length(data$m)]],start=tsp(data$y)[2]+1/frequency(data$y),f=frequency(data$y))
  colnames(tdm2.l) <- colnames(data$y)
  return(hts(y=tdm2.l, g=data$g))
} 

##############################################################################
### Conventional Top-down Forecast based on Method "A" from Gross and Sohl ###
##############################################################################

td.gsA <- function(data,h,p.f.mat,c.sum,index,scoef.l) 
{ 
  # Method "A" from Gross and Sohl implemented here
  # Construct in-sample proportion matrix
  fulldata <- allts(data)
  insamp.porps <- matrix(NA,nr=nrow(fulldata), nc=ncol(fulldata))
  for(i in 1: nrow(insamp.porps))
  {
    insamp.porps[i,1] <- 1
    if(length(data$m)>1)
    {
      for (j in 2:length(data$m))
      {
        insamp.porps[i,((index[j-1]+1):(index[j]))] <- split(fulldata[i,],rep(1:length(data$m),data$m))[[j]]/
                                                    split(fulldata[i,],rep(1:length(data$m),data$m))[[j]]%*%scoef.l[[j-1]]
      }
    }
  }
  propmat <- matrix(rep(colMeans(insamp.porps),h),h, byrow=T)
  tdmA.l <- list(matrix(p.f.mat[,1],ncol=1))
  if(length(data$m)>1)
{
    for (j in 2:length(data$m))
    {
      tmp.mat <- matrix(NA,h,data$m[[j]])
      for (i in 1:(length(c.sum[[j-1]])-1))
        tmp.mat[,(c.sum[[j-1]][i]+1):c.sum[[j-1]][i+1]] <- tdmA.l[[j-1]][,i]
      ppl.conA <- propmat[,((index[j-1]+1):(index[j]))]
      tdmA.l[[j]] <- tmp.mat * ppl.conA
    }
  }
  tdmA.l <- ts(tdmA.l[[length(data$m)]],start=tsp(data$y)[2]+1/frequency(data$y),,f=frequency(data$y))
  colnames(tdmA.l) <- colnames(data$y)
  return(hts(y=tdmA.l, g=data$g))
}

#################################################################
### Top-down forecast by Roman's Forecast Proportion Approach ###
#################################################################

td.rFP <- function(data,h,p.f.mat,c.sum,scoef.l,index) 
{ 
  # For calculating proportion we need the point forecast matrix at each level (p.f.mat.l)
  # and the proportion per level (ppl) of the hierarchy
  # and the top-down matrix per level (tdm.l) of the hierarchy
  tdm.l <- list(matrix(p.f.mat[,index[1]],h,1))
  if(length(data$m)>1)
  {
    for (j in 2:length(data$m))
    {
      tmp.mat <- matrix(NA,h,data$m[j])
      for (i in 1:ncol(tdm.l[[j-1]]))
        tmp.mat[,(c.sum[[j-1]][i]+1):(c.sum[[j-1]][i+1])] <- tdm.l[[j-1]][,i]
      p.f.mat.l <- p.f.mat[,(index[j-1]+1):index[j]]
      ppl.gs <- p.f.mat.l/(p.f.mat.l%*%scoef.l[[j-1]])
      tdm.l[[j]] <- tmp.mat*ppl.gs
    }
  }
  # The top-down matrix according to the proportion calculated by Gross and Sohl's approach
  tdm.l <- ts(tdm.l[[length(data$m)]],start=tsp(data$y)[2]+1/frequency(data$y),
  f=frequency(data$y))
  tdm.l[is.na(tdm.l)] <- 0
  colnames(tdm.l) <- colnames(data$y)
  return(hts(y=tdm.l,g=data$g))
} 

