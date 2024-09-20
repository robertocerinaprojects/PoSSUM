library(mgcv)

bias <- function(obs,pred,...){ mean( pred-obs,...) }
rmse <- function(obs,pred,...){ sqrt( mean( (obs - pred)^2,...) ) }
mae <- function(obs,pred,...){ mean(abs(obs - pred),...) }
corr <- function(obs,pred,...){ cor(x = pred,y = obs,...) }
cover <- function(obs,pred_lo,pred_hi,...){ mean( obs > pred_lo & obs < pred_hi,...) }

evals.string <- 
  function(
    obs,
    pred,
    pred_lo=NULL,
    pred_hi=NULL,
    scores = c('bias','rmse','mae','corr','cover'),
    digits = 2,
    args.mean = list(na.rm=T),
    args.cor = list(use = 'pairwise.complete.obs')
    ){
    
    evals <- c()
    for(ii in scores){
      if(!ii  %in% c('cover','corr')){
        score <- do.call(ii,c(list(obs = obs,pred = pred),args.mean))
        evals <- c(evals,paste0(ii,': ',round(score,digits)))
      }
      if(!is.null(pred_lo) & is.null(pred_hi) & (ii %in% c('cover'))){
        score <- do.call(ii,c(list(obs = obs,pred = pred),args.mean))
        evals <- c(evals,paste0(ii,': ',round(score,digits)))
      } 
      if(ii %in% c('corr')){
        score <- do.call(ii,c(list(obs = obs,pred = pred),args.cor))
        evals <- c(evals,paste0(ii,': ',round(score,digits)))
      } 
    }
    
    return(evals)
}


smooth.fit <- 
  function(
    obs,
    pred,
    smooth.family = scat(link="identity"),
    ...
    ){
    y = obs
    x = pred[[1]]
    
    fit <- 
      mgcv::gam(
        formula = obs~s(pred),
        family=smooth.family
        )
    
    
    
    }
  }


plot.obs_v_pred <- 
  function(
    obs, 
    pred, 
    jitter.factor, 
    args.plot = list(),
    args.points = list(),
    args.abline = list(a = 0, b = 1),
    args.legend = NULL
    ){
    
    if(!is.null(jitter.factor)){

      if('obs' %in% names(jitter.factor)){
        obs = jitter(obs,factor = jitter.factor[['obs']])
      }
      if('pred' %in% names(jitter.factor)){
        for(i in names(jitter.factor$pred)){
          pred[[i]] = jitter(pred[[i]],factor = jitter.factor$pred[[i]])
        }
      }     
      
    }

    do.call(plot, c(list(y = obs, x = pred[[1]]), args.plot))
    
    for(k in names(pred)[-1]){
      do.call(points, c(list(y = obs, x = pred[[k]]), args.points))
    }
    
    if(!is.null(args.abline)){
      do.call(abline, args.abline)
    }
    
    if(!is.null(args.legend)){
      do.call(legend,args.legend)
    }
    
  }
