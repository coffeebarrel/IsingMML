ising <- function(length,beta,temp,EF,enengy){
  require(reshape2);
  require(ggplot2);
  totalEnergy <- c()
  isingMatrix <- matrix(sample(c(-1,1),length^2,TRUE),length,length,TRUE);
  correl <- beta/temp;
  if(temp<5){
    step <- 70
  }
  else {step <- 25}
  sampleSize <- 100;
  
  #burn-in process
  for (count in 1:(step*length*length)){
    rownum <- sample(1:length,1);
    colnum <- sample(1:length,1);
    k <- isingMatrix[rownum,colnum];
    energy <- getEnergy(rownum,colnum,isingMatrix,correl,EF)
    if(energy>0){
      k <- -k
      }
    else {
      r <- runif(1,0,1)
      if (r<exp(energy))
      k <- -k
      }
    isingMatrix[rownum,colnum] <- k;
  }
  isingMap <- melt(isingMatrix);
  names(isingMap)=c("x","y","color");
  isingMap$color=factor(isingMap$color>0);
  graph <- qplot(x,y,fill=color,data=isingMap,geom="tile");
  print(graph);
  
  #sampleEnergy
  if(!missing(energy)){
    globalEnergy <- 0;
    for (MCcount in 1:sampleSize){
      for (count in 1:length*length){
        rownum <- sample(1:length,1);
        colnum <- sample(1:length,1);
        k <- isingMatrix[rownum,colnum];
        energy <- getEnergy(rownum,colnum,isingMatrix,correl,EF)
        if(energy>0){
          k <- -k
        }
        else {
          r <- runif(1,0,1)
          if (r<exp(energy))
            k <- -k
        }
        isingMatrix[rownum,colnum] <- k;
      }
      for (rownum in 1:length)
        for (colnum in 1:length){
          k <- isingMatrix[rownum,colnum];
          energy <- getEnergy(rownum,colnum,isingMatrix,correl,EF)
          globalEnergy <- globalEnergy+energy/2
        }
    }
  }
  globalEnergy <- globalEnergy/sampleSize
  
  return(list(isingMatrix,globalEnergy))
}
