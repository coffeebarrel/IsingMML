getEnergy <- function(rownum,colnum,isingMatrix,correl,EF){
  k <- isingMatrix[rownum,colnum]
  length <- nrow(isingMatrix)
  if (rownum==1 && colnum!=1 && colnum!=length){
  #print(c(rownum,colnum,1));
  kl <- isingMatrix[rownum,colnum-1];
  kr <- isingMatrix[rownum,colnum+1];
  kd <- isingMatrix[rownum+1,colnum];
  energy <- -correl*k*sum(kl,kr,kd)-EF*k;
}
  
  else if (rownum==length && colnum!=1 && colnum!=length){
    kl <- isingMatrix[rownum,colnum-1];
    kr <- isingMatrix[rownum,colnum+1];
    ku <- isingMatrix[rownum-1,colnum];
    energy <- -correl*k*sum(kl,kr,ku)-EF*k;
}
else if (colnum==1 && rownum!=1 && rownum!=length){
  
  ku <- isingMatrix[rownum-1,colnum];
  kr <- isingMatrix[rownum,colnum+1];
  kd <- isingMatrix[rownum+1,colnum];
  energy <- -correl*k*sum(ku,kr,kd)-EF*k;
}
else if (colnum==length && rownum!=1 && rownum!=length){
  ku <- isingMatrix[rownum-1,colnum];
  kl <- isingMatrix[rownum,colnum-1];
  kd <- isingMatrix[rownum+1,colnum];
  energy <- -correl*k*sum(kl,ku,kd)-EF*k;
}
else if (colnum==1 && rownum==1){
  kr <- isingMatrix[rownum,colnum+1];
  kd <- isingMatrix[rownum+1,colnum];
  energy <- -correl*k*sum(kr,kd)-EF*k;
}
else if (colnum==1 && rownum==length){
  kr <- isingMatrix[rownum,colnum+1];
  ku <- isingMatrix[rownum-1,colnum];
  energy <- -correl*k*sum(ku,kr)-EF*k;
}
else if (colnum==length && rownum==1){
  kl <- isingMatrix[rownum,colnum-1];
  kd <- isingMatrix[rownum+1,colnum];
  energy <- -correl*k*sum(kl,kd)-EF*k;
}
else if (colnum==length && rownum==length){
  kl <- isingMatrix[rownum,colnum-1];
  ku <- isingMatrix[rownum-1,colnum];
  energy <- -correl*k*sum(kl,ku)-EF*k;
}
else {
  kl <- isingMatrix[rownum,colnum-1];
  kr <- isingMatrix[rownum,colnum+1];
  ku <- isingMatrix[rownum-1,colnum];
  kd <- isingMatrix[rownum+1,colnum]
  energy <- -correl*k*sum(kl,kr,kd,ku)-EF*k;
}}