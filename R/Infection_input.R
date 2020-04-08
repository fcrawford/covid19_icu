
############## change infections to ED visits


#' @export
reporting_infections<- function(              
  params,
  doprotocols=0,
  #####################
  infection_timeseries= c(0,0,0),
  reporting_rate,
  reporting_percentage=0.1,
  ...
){
 
  init       <- c(ED=0)
  
  infections <- approxfun(
    infection_timeseries*reporting_percentage,
    rule=2)
  
  model <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      ED = state[1]
     
      dED  <-   reporting_rate * infections(time) 
      
      return(list(dED)
          
        )
    })
  }
  
  
  out <- as.data.frame(ode(y=init, times= c(1:length(infection_timeseries)), func=model, parms=params, method="lsodes"))
  names(out)[2:ncol(out)] = names(init)
  #print(proc.time() - ptm)
  
  return(out)
  
}
