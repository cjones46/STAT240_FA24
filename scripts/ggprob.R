require(tidyverse)

### binomial
gbinom_default_a = function(n, p, scale = FALSE)
{
  a = ifelse(scale,
             floor(n*p - 4*sqrt(n*p*(1-p))),0)
  return (a)
}

gbinom_default_b = function(n, p, scale = FALSE)
{
  b = ifelse(scale,
             floor(n*p + 4*sqrt(n*p*(1-p))),n)
  return (b)
}

geom_binom_density = function(n = 1, p = 0.5, scale = FALSE, a=NULL, b=NULL, color="blue", ...)
{
  if ( is.null(a) )
  {
    a = gbinom_default_a(n, p, scale)
  }
  if ( is.null(b) )
  {
    b = gbinom_default_b(n, p, scale)
  }
  # make sure a and b are integers
  a = round(a)
  b = round(b)
  # make sure a < b
  if(a > b) {
    temp = a
    a = b
    b = temp
  }
  # make sure a and b are in range
  if(a < 0)
    a = 0
  if(b > n)
    b = n

  dat = tibble( x = seq(a,b,1),
                xend = x,
                y = dbinom(x,n,p),
                yend = rep(0, length(y)))

  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), data = dat, color = color, ...)
}

gbinom = function(n,p,scale = FALSE,
                  a = gbinom_default_a(n,p,scale),
                  b = gbinom_default_b(n,p,scale),
                  color = "blue",
                  title = TRUE, ...) {

  g = ggplot()

  g = g +
    geom_binom_density(n, p, scale, a, b, color, ...) +
    xlab('x') +
    ylab('Probability') +
    geom_hline(yintercept=0)

  if ( title )
  {
    g = g + ggtitle(  paste("Binomial(",n,",",p,")") )
  }

  return( g )
}

### normal
geom_norm_density = function(mu=0,sigma=1,a=NULL,b=NULL,color="blue",...)
{
  if ( is.null(a) )
  {
    a = qnorm(0.0001,mu,sigma)
  }
  if ( is.null(b) )
  {
    b = qnorm(0.9999,mu,sigma)
  }
  x = seq(a,b,length.out=1001)
  df = data.frame(
    x=x,
    y=dnorm(x,mu,sigma)
  )
  geom_line(aes(x=x,y=y), data = df, color=color,...)
}

geom_norm_fill = function(mu=0,sigma=1,a=NULL,b=NULL,
                          fill="firebrick4",...)
{
  if ( is.null(a) )
  {
    a = qnorm(0.0001,mu,sigma)
  }
  if ( is.null(b) )
  {
    b = qnorm(0.9999,mu,sigma)
  }
  x = seq(a,b,length.out=1001)
  df = data.frame(
    x=x,
    ymin=rep(0,length(x)),
    ymax = dnorm(x,mu,sigma)
  )
  geom_ribbon(aes(x=x,ymin=ymin,ymax=ymax,y=NULL), data = df, fill = fill, ...)
}

gnorm = function(mu=0,sigma=1,a=NULL,b=NULL,color="blue",
                 fill=NULL,title=TRUE,...)
{
  g = ggplot()

  if ( !is.null(fill) )
    g = g + geom_norm_fill(mu,sigma,a,b,fill)

  g = g +
    geom_norm_density(mu,sigma,a,b,color,...) +
    geom_hline(yintercept=0) +
    ylab('density')

  if ( title )
    g = g +
    ggtitle(paste("N(",mu,",",sigma,")"))
  return ( g )
}

### chi-square
geom_chisq_null_a = function(df)
{
  if ( df < 2 )
    a = qchisq(0.05,df)
  else
    a = qchisq(0.0001,df)
  return ( a )
}

geom_chisq_null_b = function(df)
{
  if ( df < 2 )
    b = qchisq(0.95,df)
  else
    b = qchisq(0.9999,df)
  return ( b )
}

geom_chisq_density = function(df=1,a=NULL,b=NULL,color="blue",...)
{
  if ( is.null(a) )
    a = geom_chisq_null_a(df)
  if ( is.null(b) )
    b = geom_chisq_null_b(df)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    y=dchisq(x,df)
  )
  geom_line(data=dat,aes(x=x,y=y),color=color,...)
}

geom_chisq_fill = function(df=1,a=NULL,b=NULL,
                           fill="firebrick4",...)
{
  if ( is.null(a) )
    a = geom_chisq_null_a(df)
  if ( is.null(b) )
    b = geom_chisq_null_b(df)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    ymin=rep(0,length(x)),
    ymax = dchisq(x,df)
  )
  geom_ribbon(data=dat,aes(x=x,ymin=ymin,ymax=ymax,y=NULL),fill=fill,...)
}

gchisq = function(df=1,a=NULL,b=NULL,color="blue",
                  fill=NULL,title=TRUE,...)
{
  g = ggplot()

  if ( !is.null(fill) )
    g = g + geom_chisq_fill(df,a,b,fill)

  g = g +
    geom_chisq_density(df,a,b,color,...) +
    geom_hline(yintercept=0) +
    ylab('density')

  if ( title )
    g = g +
    ggtitle(paste("Chi-square(",df,")"))
  return ( g )
}

### t
geom_t_density = function(df=1,a=NULL,b=NULL,color="blue",...)
{
  if ( is.null(a) )
    a = qt(0.0001,df)
  if ( is.null(b) )
    b = qt(0.9999,df)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    y=dt(x,df)
  )
  geom_line(data=dat,aes(x=x,y=y),color=color,...)
}

geom_t_fill = function(df=1,a=NULL,b=NULL,
                       fill="firebrick4",...)
{
  if ( is.null(a) )
    a = qt(0.0001,df)
  if ( is.null(b) )
    b = qt(0.9999,df)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    ymin=rep(0,length(x)),
    ymax = dt(x,df)
  )
  geom_ribbon(data=dat,aes(x=x,ymin=ymin,ymax=ymax,y=NULL),fill=fill,...)
}

gt = function(df=1,a=NULL,b=NULL,color="blue",
              fill=NULL,title=TRUE,...)
{
  g = ggplot()

  if ( !is.null(fill) )
    g = g + geom_t_fill(df,a,b,fill)

  g = g +
    geom_t_density(df,a,b,color,...) +
    geom_hline(yintercept=0) +
    ylab('density')

  if ( title )
    g = g +
    ggtitle(paste("t(",round(df,3),")"))
  return ( g )
}

### F
geom_f_density = function(df1=1,df2=1,a=NULL,b=NULL,color="blue",...)
{
  if ( is.null(a) )
    a = qf(0.0001,df1,df2)
  if ( is.null(b) )
    b = qf(0.9999,df1,df2)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    y=df(x,df1,df2)
  )
  geom_line(data=dat,aes(x=x,y=y),color=color,...)
}

geom_f_fill = function(d1f=1,df2=1,a=NULL,b=NULL,
                       fill="firebrick4",...)
{
  if ( is.null(a) )
    a = qf(0.0001,df1,df2)
  if ( is.null(b) )
    b = qf(0.9999,df1,df2)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    ymin=rep(0,length(x)),
    ymax = df(x,df1,df2)
  )
  geom_ribbon(data=dat,aes(x=x,ymin=ymin,ymax=ymax,y=NULL),fill=fill,...)
}

gf = function(df1=1,df2=1,a=NULL,b=NULL,color="blue",
              fill=NULL,title=TRUE,...)
{
  g = ggplot()

  if ( !is.null(fill) )
    g = g + geom_f_fill(df1,df2,a,b,fill)

  g = g +
    geom_f_density(df1,df2,a,b,color,...) +
    geom_hline(yintercept=0) +
    ylab('density')

  if ( title )
    g = g +
    ggtitle(paste("F(",df1,",",df2,")"))
  return ( g )
}

### beta
geom_beta_null_a = function(alpha,beta)
{
  if ( alpha >= 1)
    a = 0
  else
    a = 0.01
  return ( a )
}

geom_beta_null_b = function(alpha,beta)
{
  if ( beta >= 1 )
    b = 1
  else
    b = 0.99
  return ( b )
}

geom_beta_density = function(alpha=1, beta=1, a=NULL, b=NULL, color="blue",...)
{
  if ( is.null(a) )
    a = geom_beta_null_a(alpha,beta)
  if ( is.null(b) )
    b = geom_beta_null_b(alpha,beta)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    y=dbeta(x,alpha,beta)
  )
  geom_line(data=dat,aes(x=x,y=y),color=color,...)
}

geom_beta_fill = function(alpha=1,beta=1,a=NULL,b=NULL,
                           fill="firebrick4",...)
{
  if ( is.null(a) )
    a = geom_beta_null_a(alpha,beta)
  if ( is.null(b) )
    b = geom_beta_null_b(alpha,beta)
  x = seq(a,b,length.out=1001)
  dat = data.frame(
    x=x,
    ymin=rep(0,length(x)),
    ymax = dbeta(x,alpha,beta)
  )
  geom_ribbon(data=dat,aes(x=x,ymin=ymin,ymax=ymax,y=NULL),fill=fill,...)
}

gbeta = function(alpha=1,beta=1,a=NULL,b=NULL,color="blue",
                  fill=NULL,title=TRUE,...)
{
  g = ggplot()

  if ( !is.null(fill) )
    g = g + geom_beta_fill(alpha,beta,a,b,fill)

  g = g +
    geom_beta_density(alpha,beta,a,b,color,...) +
    geom_hline(yintercept=0) +
    ylab('density')

  if ( title )
    g = g +
    ggtitle(paste("Beta(",alpha,",",beta,")"))
  return ( g )
}
