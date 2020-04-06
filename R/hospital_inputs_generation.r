#' @export
report_rate<-function(t,
                      initial_report, 
                      final_report, 
                      distribution,
                      growth_rate,
                      rampslope){
  
  
  ############## Reporting rate function determines who shows up to the ED 
  
  report_rate<-rep(0,t)
  if (distribution=="uniform"){
    report_rate<-rep(initial_report,t)
  }
  if (distribution=="logistic"){
    z <- log(1/0.005-1)
    zz  <- seq(-z*(1+(2/(t-3))),
               z*(1+(2/(t-3))),
               by=(2*z)/(t-3))
    zzz  <- as.numeric(final_report-initial_report)/(1+exp(-zz))
    report_rate<-zzz+initial_report
  }
  if (distribution=="ramp"){
    times = seq(1, t, by=1)
    report_rate<- initial_report + rampslope*times
  }
  
  if (distribution=="geometric"){
    geometric_factor<- exp(1/t* log(final_report/initial_report))
    report_rate<- (geometric_factor^(1:t))*initial_report	
  }
  
  if (distribution=="exponential"){
    
    
    report_rate<- (exp(growth_rate*(1:t)))*initial_report	
  }
  
  try(if (length(report_rate) != t)(stop("reporting rate time scale does not match inputted timescale")))
  
  return(report_rate)
}


# capacity ramp building
#' @export
capacity_ramping<-function(start,
                           finish,
                           ramp,
                           t){
  capacity <- rep(start, t)
  if (ramp[1]!=0){
    capacity[ramp[1]:ramp[2]]= start + (finish-start)* (0:(ramp[2]-ramp[1]))/(ramp[2]-ramp[1]);
    capacity[ramp[2]:t] = finish;
  } else if (ramp[2]!=0){
    capacity[(ramp[1]+1):ramp[2]]= start + (finish-start)* (1:(ramp[2]-ramp[1]))/(ramp[2]-ramp[1]);
    capacity[ramp[2]:t] = finish;
  } else{
    capacity[1:t] = finish;
    
  }
  
  
  
  
  capacity
  
}

#' @export
hospital_input_generation <- function(dynamicModel=0,
                                      t,
                                      I_init,
                                      I_final,
                                      distribution,
                                      doublingtime,
                                      rampslope,
                                      ed_visit_timeseries
){
  
  if (dynamicModel==0)
    output<- report_rate(
      t = t, 
      initial_report = I_init, 
      final_report = I_final, 
      distribution=distribution, 
      growth_rate=log(2)/doublingtime, 
      rampslope=rampslope
    ) else{
      output<- ed_visit_timeseries
      
    }
  output
  
}

#' @export
floor_capacity_timeseries <- function(t,
                                      L,
                                      L_occupied,
                                      L_final,
                                      L_ramp, 
                                      doprotocols=0
){
  
  output<- capacity_ramping(
    start=L,
    finish=L_final,
    ramp=c(L_ramp[1],L_ramp[2]),
    t=t)
  

  
  output
}





icu_capacity_timeseries <- function(t,
                                    M,
                                    M_occupied,
                                    M_final,
                                    M_ramp,
                                    doprotocols=0){
  
  output<- capacity_ramping(
    start=M,
    finish=M_final,
    ramp=c(M_ramp[1],M_ramp[2]),
    t=t)
  
  output
  
  
}
