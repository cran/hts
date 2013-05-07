############################
###### Bottom-up Code ######
############################
   
bup.f <- function (data, h, p.f.mat, index) 
{
  a <- length(index)
  p.f.m.bu <- p.f.mat[, (index[a-1] + 1):index[a],drop=FALSE]
  p.f.m.bu <- ts(matrix(p.f.m.bu, h, data$m[length(data$m)]), start = tsp(data$y)[2]+1/frequency(data$y),
				  frequency = frequency(data$y))
  colnames(p.f.m.bu) <- colnames(data$y)
  data$oldy <- data$y
  data$y <- p.f.m.bu
	return(data)
}
