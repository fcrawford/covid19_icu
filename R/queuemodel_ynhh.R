
#*************************** COVID-19 Hospital Queueing MODEL *****************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#

# lambda = rate of presenting for care
# M = number of ICU beds
# L = number of Floor beds
# eta = rate of movement to an ICU bed from queue
# zeta = rate of movement to a floor bed from queue

############## Reporting rate function determines who shows up to the ED 3/23
#' @export
report_rate<-function(t,
                          initial_report, 
                          final_report, 
                          distribution,
                          growth_rate,
                          rampslope){
  
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


empirical_ynhh<- function ()
{
  patients = read.csv( system.file("content/EDvisits_YNHHS_04042020.csv", package='covid19icu') )

  
  list(time=patients$X, young=patients$Number1, medium=patients$Number2, old=patients$Number3)
}
############## run the queuing model


#' @export
hospital_queues_ynhh<- function(t,
                                         young,
                                         medium,
                                         #######################
                                         I_init,
                                         I_final,
                                         distribution,
                                         doublingtime,
                                         rampslope,
                                         #######################
                                         M,
                                         L,
                                         L_occupied,
                                         M_occupied,
                                         Lfinal,
                                         Lramp,
                                         Mfinal,
                                         Mramp,
                                         ######################
                                         avg_LOS_ICU,
                                         avg_LOS_Floor,
                                         #####################
                                         p_death_ICU2,
                                         p_death_ICU3,
                                         p_death_floor2,
                                         p_death_floor3,
                                         #####################
                                         slope,
                                         doprotocols=0,
                                        read_in=0,
                                         ...
){
  

  
  
  # read in fixed and derived parameters
  
  params = update_inputs(t,
                                       young,
                                       medium,
                                       #######################
                                       I_init,
                                       I_final,
                                       distribution,
                                       doublingtime,
                                       rampslope,
                                       #######################
                                       M,
                                       L,
                                       L_occupied,
                                       M_occupied,
                                       Lramp,
                                       Mramp,
                                       ######################
                                       avg_LOS_ICU,
                                       avg_LOS_Floor,
                                       #####################
                                       p_death_ICU2,
                                       p_death_ICU3,
                                       p_death_floor2,
                                       p_death_floor3,
                                       #####################
                                       slope
                                       );
  
  if(doprotocols==0) {
    params$Mfinal=params$M
    params$Lfinal=params$L
  } else{
    params$Mfinal=Mfinal
    params$Lfinal=Lfinal
    
    
  }
  
  
  ############## SET INITIAL CONDITIONS
  
  
  
  
  #ptm <- proc.time()
  
  ##########
  
  init       <- c(I=rep(0.0, times=3),
                  P=c(params$young, params$medium, params$old) * params$I_init, 
                  MS=rep(0.0, times=3),
                  WC=rep(0.0, times=3),
                  C=c(params$young, params$medium, params$old) * params$M * params$M_occupied/100,
                  WF=rep(0.0, times=3),
                  FL=c(params$young, params$medium, params$old) * params$L * params$L_occupied/100,
                  R=rep(0.0, times=3),
                  D=rep(0.0, times=3),
                  Dead_at_ICU =0,
                  Dead_on_Floor =0,
                  Dead_waiting_for_ICU =0,
                  Dead_waiting_for_Floor=0,
                  Dead_with_mild_symptoms=0,
                  Dead_in_ED=0,
                  Number_seen_at_ED=0,
                  CTotal= params$M * params$M_occupied/100,
                  FTotal= params$L * params$L_occupied/100
  )
  
  ### create functions for reports and ramping
  if (read_in==0)
  {
    
    reports <- approxfun(
      report_rate(
        t = params$t,
        initial_report = params$I_init,
        final_report = params$I_final,
        distribution=params$distribution,
        growth_rate=log(2)/params$doublingtime,
        rampslope=params$rampslope
      ),
      rule=2)
    

    
    youngs <-  function(time){
      reports(time) * params$young
    }
    
    mediums <- function(time){
      reports(time) * params$medium
    }
    olds <- function(time){
      reports(time) * params$old
    }
    
  } else{
    reports <- empirical_ynhh();
    
    params$t=max(reports$time)
    
    youngs <- approxfun( 
      x= reports$time,
      y= reports$young,
      method="constant",
      f=0,
      rule=2);
    
    mediums <- approxfun( 
      x= reports$time,
      y= reports$medium,
      method="constant",
      f=0,
      rule=2);
    
    
    olds <- approxfun( 
      x= reports$time,
      y= reports$old,
      method="constant",
      f=0,
      rule=2);
    
    
    
    
  }

  
  
  
  capacity_L <- approxfun(
    capacity_ramping(
      start=params$L,
      finish=params$Lfinal,
      ramp=c(params$floorcapramp1,params$floorcapramp2),
      t=params$t),
    rule=2);
  
  capacity_M <- approxfun(
    capacity_ramping(
      start=params$M,
      finish=params$Mfinal,
      ramp=c(params$icucapramp1,params$icucapramp2),
      t=params$t),
    rule=2);
  
  
  
  model <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      I = state[1:3]
      P = state[4:6]
      MS = state[7:9]
      WC = state[10:12]
      C = state[13:15]
      WF = state[16:18]
      FL = state[19:21]
      R = state[22:24]
      D = state[25:27]
      Dead_at_ICU= state[28];
      Dead_on_Floor= state[29];
      Dead_waiting_for_ICU= state[30];
      Dead_waiting_for_Floor= state[31];
      Dead_with_mild_symptoms= state[32];
      Dead_in_ED= state[33];
      Number_seen_at_ED= state[34];
      CTotal= state[35]
      FTotal= state[36]
      
      C_capped = 1-1/(1+exp(slope*(CTotal -capacity_M(time))))
      F_capped = 1-1/(1+exp(slope*(FTotal -capacity_L(time))))
      
      reporting = c(youngs(time), mediums(time), olds(time))
      
      
      dI  <- c(0,0,0) # - lambda* I - phi_I * I - mu_I * I 
      dP  <- -(sigma_MS+sigma_C+sigma_F+mu_P)*P + xi_MS * MS + reporting # + lambda_I * I
      dMS <- sigma_MS * P - (phi + mu_MS + xi_MS)* MS
      dWC <- (theta_WF * WF + theta_F * FL + sigma_C * P)*C_capped - eta*WC *(1-C_capped) -mu_WC * WC
      dC  <- (theta_WF * WF + theta_F * FL + sigma_C * P + eta*WC)* (1-C_capped) - mu_C*C - chi_C *C
      dWF <- (sigma_F* P + chi_C*C) * F_capped - zeta * WF * (1-F_capped) - (mu_WF+ theta_WF+chi_LQ)* WF
      dFL <- (sigma_F* P + chi_C*C + zeta*WF) * (1-F_capped) - (mu_F + theta_F+chi_L)* FL
      dR  <- phi* MS + chi_L* FL + phi_I * I + chi_LQ * WF
      dD  <- mu_C * C+ mu_F * FL + mu_I * I + mu_MS *MS + mu_WF * WF + mu_WC * WC + mu_P * P

      dDead_at_ICU = mu_C %*% C;
      dDead_on_Floor = mu_F %*% FL ;
      dDead_waiting_for_ICU = mu_WC %*% WC;
      dDead_waiting_for_Floor =  mu_WF %*% WF;
      dDead_with_mild_symptoms = mu_MS %*%MS;
      dDead_in_ED = mu_P %*% P;
      dNumber_seen_at_ED = sum(reporting)+xi_MS %*% MS; #reports(time)  
      
      dCTotaldt = sum((theta_WF * WF + theta_F * FL + sigma_C * P + eta*WC)* (1-C_capped) - mu_C*C - chi_C *C)
      dFTotaldt = sum((sigma_F* P + chi_C*C + zeta*WF) * (1-F_capped) - (mu_F+ theta_F+chi_L)* FL) 
      
      return(
        list(
          c(dI,dP,dMS, dWC,dC, 
            dWF, dFL, dR, dD, 
            dDead_at_ICU, dDead_on_Floor,dDead_waiting_for_ICU, 
            dDead_waiting_for_Floor,dDead_with_mild_symptoms, dDead_in_ED, 
            dNumber_seen_at_ED, dCTotaldt, dFTotaldt
          )
      
        )
      )
    })
  }
  
  
  out <- as.data.frame(ode(y=init, times= c(1:params$t), func=model, parms=params, method="lsodes"))
  names(out)[2:ncol(out)] = names(init)
  #print(proc.time() - ptm)
  
  out$reports <- youngs(1:params$t)+mediums(1:params$t)+olds(1:params$t);
  out$capacity_L <- capacity_L(1:params$t);
  out$capacity_M <- capacity_M(1:params$t);
  
  
  return(out)
  
}
