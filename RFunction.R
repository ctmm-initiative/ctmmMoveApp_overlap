library(ctmm)

rFunction = function(data) {
  
  ov <- overlap(data[[2]]) 
  
  res <- apply(ov$CI, 1:2, function(x) {
    x <- format(round(x, 2), nsmall = 2)
    paste0(x[2], " (", x[1], " - ", x[3], ")")
  })
  
  capture.output(res, file = appArtifactPath("overlap_summary.txt"))
  
  return(data)
  
}