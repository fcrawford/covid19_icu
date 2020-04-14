
############## change infections to ED visits


#' @export
reporting_infections<- function(              
  params,
  ...
){
 
  init       <- c(I=params$average_starting_infectives,
                  ED=0)
  
  params$t=length( params$infection_timeseries)
  
  infections <- approxfun(
    params$infection_timeseries,
    rule=2)
  print(params$infection_timeseries)
  
  model <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      I = state[1]
      ED = state[2]
      
      dI <- - 1/average_reporting_delay * I + infections(time);
      dED  <-   1/average_reporting_delay * average_reporting_percentage/100 * I;

          return(list(c(dI, dED))
          
        )
    })
  }
  
  
  out <- as.data.frame(ode(y=init, times= c(0:params$t), func=model, parms=params, method="lsodes"))
  names(out)[2:ncol(out)] = names(init)
  #print(proc.time() - ptm)
  
  return(diff(out$ED, lag=1))
  
}
