lethal_proportion<-function(x,y) 
{
  keep=!is.na(x)
  x=x[keep]
  y=y[keep]
  rho=max((0.25-mean(x))/(3.65/4-0.125),0)
  3.65*rho
}

bootstrap<-function(x,y)
{
  n=5000
  boot_sample=c()
  for (i in 1:n)
  {
    tmp=sample(1:length(x),length(x),replace=T)
    x0=x[tmp]
    y0=y[tmp]
    boot_sample=c(boot_sample,lethal_proportion(x0,y0))
  }
  quantile(boot_sample,probs=c(0.025,0.975))
}