#' @export
update_inputs <- function(t,
                          young,#
                          medium,#
                          #######################
                          I_init,#
                          I_final,#
                          distribution,#
                          doublingtime,#
                          rampslope,#
                          #######################
                          M,#
                          L,#
                          L_occupied,#
                          M_occupied,#
                          Lramp,#
                          Mramp,#
                          ######################
                          avg_LOS_ICU,#
                          avg_LOS_Floor,#
                          #####################
                          p_death_ICU2,#
                          p_death_ICU3,#
                          p_death_floor2,#
                          p_death_floor3,#
                          #####################
                          slope,
                          #####################
                          ed_visits_timeseries,
                          #####################
                          M_final,
                          L_final,
                          #####################
                          reporting_delay,
                          reporting_percentage,
                          starting_infectives,
                          infection_timeseries,
                          #####################
                          agespecificLOS,
                          avg_LOS_ICU1,
                          avg_LOS_Floor1,
                          avg_LOS_ICU2,
                          avg_LOS_Floor2,
                          avg_LOS_ICU3,
                          avg_LOS_Floor3,
                          ... #
                          ) {
  
  params = yaml.load_file( system.file("content/parameter_values.yaml", package='covid19icu') )
  
  ## Initializing the parameters not set
  
  if(!missing(t)) params$t=t
  if(!missing(agespecificLOS)) params$agespecificLOS=agespecificLOS
  

    
  if(!missing(young)) params$young=young;
  if(!missing(medium)) params$medium=medium;
  params$old = 1- params$young - params$medium
  
  ###########################################
  
  
  if(!missing(I_init)) params$I_init=I_init;
  if(!missing(I_final)) params$I_final=I_final;
  if(!missing(distribution)) params$distribution=distribution;
  if(!missing(doublingtime)) params$doublingtime=doublingtime;
  if(!missing(rampslope)) params$rampslope=rampslope;
  
  
  ###########################################
  if(!missing(M)) params$M=M;
  if(!missing(L)) params$L=L;
  
  
  if(!missing(M_occupied)) params$M_occupied=M_occupied;
  if(!missing(L_occupied)) params$L_occupied=L_occupied;
  
  if(!missing(Mramp)) 
  {
    params$icucapramp1=Mramp[1];
    params$icucapramp2=Mramp[2];
  }
  if(!missing(Lramp)) 
  {
    params$floorcapramp1=Lramp[1];
    params$floorcapramp2=Lramp[2];
  }
  
  if(!missing(M_final)) params$M_final=M_final;
  if(!missing(L_final)) params$L_final=L_final;
  
  ###########################################
  if(params$agespecificLOS==0) 
  {
    if(!missing(avg_LOS_ICU)) 
    {
      params$avg_LOS_ICU1=avg_LOS_ICU;
      params$avg_LOS_ICU2=avg_LOS_ICU;
      params$avg_LOS_ICU3=avg_LOS_ICU;
      params$avg_LOS_ICU = avg_LOS_ICU;
      
    }
    if(!missing(avg_LOS_Floor)) 
    {
      params$avg_LOS_Floor1=avg_LOS_Floor;
      params$avg_LOS_Floor2=avg_LOS_Floor;
      params$avg_LOS_Floor3=avg_LOS_Floor;
      params$avg_LOS_Floor = avg_LOS_Floor;
      params$avg_LOS_FloorQ1=avg_LOS_Floor
      params$avg_LOS_FloorQ2=avg_LOS_Floor
      params$avg_LOS_FloorQ3=avg_LOS_Floor
    }
    
  }
  
  if(params$agespecificLOS==1) 
  {
    
    if(!missing(avg_LOS_ICU1)) params$avg_LOS_ICU1=avg_LOS_ICU1;
    if(!missing(avg_LOS_ICU2)) params$avg_LOS_ICU2=avg_LOS_ICU2;
    if(!missing(avg_LOS_ICU3)) params$avg_LOS_ICU3=avg_LOS_ICU3;
    params$avg_LOS_ICU = params$young*params$avg_LOS_ICU1+params$medium*params$avg_LOS_ICU2+params$old*params$avg_LOS_ICU3;
    if(!missing(avg_LOS_Floor1)) params$avg_LOS_Floor1=avg_LOS_Floor1;
    if(!missing(avg_LOS_Floor2)) params$avg_LOS_Floor2=avg_LOS_Floor2;
    if(!missing(avg_LOS_Floor3)) params$avg_LOS_Floor3=avg_LOS_Floor3;
    params$avg_LOS_Floor = params$young*params$avg_LOS_Floor1+params$medium*params$avg_LOS_Floor2+params$old*params$avg_LOS_Floor3;
    if(!missing(avg_LOS_Floor1)) params$avg_LOS_FloorQ1=avg_LOS_Floor1;
    if(!missing(avg_LOS_Floor2)) params$avg_LOS_FloorQ2=avg_LOS_Floor2;
    if(!missing(avg_LOS_Floor3)) params$avg_LOS_FloorQ3=avg_LOS_Floor3;
  }

  ###########################################
  
  if(!missing(p_death_ICU2)) params$p_death_ICU2=p_death_ICU2;
  if(!missing(p_death_ICU3)) params$p_death_ICU3=p_death_ICU3;
  if(!missing(p_death_floor2)) params$p_death_Floor2=p_death_floor2;
  if(!missing(p_death_floor3)) params$p_death_Floor3=p_death_floor3;
  
  if(!missing(p_death_floor2)) params$p_death_WF2=p_death_floor2;
  if(!missing(p_death_floor3)) params$p_death_WF3=p_death_floor3;

  ###########################################

  if(!missing(slope)) params$slope=slope;
  
  ########################################## NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW 
  if(!missing(ed_visits_timeseries)) {
    params$ed_visits_timeseries = ed_visits_timeseries;
  } else {
    
    params$ed_visits_timeseries=rep(0,20);
    
  }
  
  if(!missing(infection_timeseries)) {
    params$infection_timeseries = infection_timeseries;
  } else {
    
    params$infection_timeseries=rep(0,20);
    
  }
  
  if(!missing(reporting_delay)) params$average_reporting_delay=reporting_delay;
  if(!missing(reporting_percentage)) params$average_reporting_percentage=reporting_percentage;
  if(!missing(starting_infectives)) params$average_starting_infectives=starting_infectives;
  
  ###########################################
  
  ## Leaving ED
  
  params$sigma_MS1= params$percentage_MS1 * 1/params$t_Triage;
  params$sigma_MS2= params$percentage_MS2 * 1/params$t_Triage;
  params$sigma_MS3= params$percentage_MS3 * 1/params$t_Triage;
  params$sigma_F1= params$percentage_F1 * 1/params$t_Triage;
  params$sigma_F2= params$percentage_F2 * 1/params$t_Triage;
  params$sigma_F3= params$percentage_F3 * 1/params$t_Triage;
  params$sigma_C1= params$percentage_C1 * 1/params$t_Triage;
  params$sigma_C2= params$percentage_C2 * 1/params$t_Triage;
  params$sigma_C3= params$percentage_C3 * 1/params$t_Triage;
  
  # Leaving ICU
  params$mu_C1= params$p_death_ICU1 * 1/params$avg_LOS_ICU1;
  params$chi_C1= (1-params$p_death_ICU1) * 1/params$avg_LOS_ICU1;

  params$mu_C2= params$p_death_ICU2 * 1/params$avg_LOS_ICU2;
  params$chi_C2= (1-params$p_death_ICU2) * 1/params$avg_LOS_ICU2;
  
  params$mu_C3= params$p_death_ICU3 * 1/params$avg_LOS_ICU3;
  params$chi_C3= (1-params$p_death_ICU3) * 1/params$avg_LOS_ICU3;
  
  ######################
  # Leaving Floor
  params$mu_F1= params$p_death_Floor1 * 1/params$avg_LOS_Floor1;
  params$theta_F1= params$p_stepup_Floor1 * 1/params$avg_LOS_Floor1;
  params$chi_L1= (1-params$p_death_Floor1 - params$p_stepup_Floor1 )* 1/params$avg_LOS_Floor1;
  
  params$mu_F2= params$p_death_Floor2 * 1/params$avg_LOS_Floor2;
  params$theta_F2= params$p_stepup_Floor2 * 1/params$avg_LOS_Floor2;
  params$chi_L2= (1-params$p_death_Floor2 - params$p_stepup_Floor2 )* 1/params$avg_LOS_Floor2;
  
  params$mu_F3= params$p_death_Floor3 * 1/params$avg_LOS_Floor3;
  params$theta_F3= params$p_stepup_Floor3 * 1/params$avg_LOS_Floor3;
  params$chi_L3= (1-params$p_death_Floor3 - params$p_stepup_Floor3 )* 1/params$avg_LOS_Floor3;


  ######################
  # Leaving MS
  params$mu_MS1= params$p_death_MS1 * 1/params$avg_T_exit1;
  params$xi_MS1= params$p_return_MS1 * 1/params$avg_T_exit1;
  params$phi1= (1-params$p_death_MS1 - params$p_return_MS1 )* 1/params$avg_T_exit1;
  
  params$mu_MS2= params$p_death_MS2 * 1/params$avg_T_exit2;
  params$xi_MS2= params$p_return_MS2 * 1/params$avg_T_exit2;
  params$phi2= (1-params$p_death_MS2 - params$p_return_MS2 )* 1/params$avg_T_exit2;
  
  params$mu_MS3= params$p_death_MS3 * 1/params$avg_T_exit3;
  params$xi_MS3= params$p_return_MS3 * 1/params$avg_T_exit3;
  params$phi3= (1-params$p_death_MS3 - params$p_return_MS3 )* 1/params$avg_T_exit3;
  
  ######################
  # Leaving FL Queue
  params$mu_WF1= params$p_death_WF1 * 1/params$avg_LOS_FloorQ1;
  params$theta_WF1= params$p_stepup_WF1 * 1/params$avg_LOS_FloorQ1;
  params$chi_LQ1= (1-params$p_death_WF1 - params$p_stepup_WF1 )* 1/params$avg_LOS_FloorQ1;
  
  params$mu_WF2= params$p_death_WF2 * 1/params$avg_LOS_FloorQ2;
  params$theta_WF2= params$p_stepup_WF2 * 1/params$avg_LOS_FloorQ2;
  params$chi_LQ2= (1-params$p_death_WF2 - params$p_stepup_WF2 )* 1/params$avg_LOS_FloorQ2;
  
  params$mu_WF3= params$p_death_WF3 * 1/params$avg_LOS_FloorQ3;
  params$theta_WF3= params$p_stepup_WF3 * 1/params$avg_LOS_FloorQ3;
  params$chi_LQ3= (1-params$p_death_WF3 - params$p_stepup_WF3 )* 1/params$avg_LOS_FloorQ3;
  

  ## Fast emptying of queue
  params$zeta1 <- params$p_tofloor1 * (params$mu_WF1+params$theta_WF1+ params$chi_LQ1)/(1- params$p_tofloor1)
  params$zeta2 <- params$p_tofloor2 * (params$mu_WF2+params$theta_WF2+ params$chi_LQ2)/(1- params$p_tofloor2)
  params$zeta3 <- params$p_tofloor3 * (params$mu_WF3+params$theta_WF3+ params$chi_LQ3)/(1- params$p_tofloor3)
  
  ######################
  # Leaving ICU Queue
  params$mu_WC1 <- 1/params$avg_timetodeath_CQ1
  params$mu_WC2 <- 1/params$avg_timetodeath_CQ2
  params$mu_WC3 <- 1/params$avg_timetodeath_CQ3
  
  ## Fast emptying of queue
  params$eta1 <- params$p_toicu1 * (params$mu_WC1)/(1- params$p_toicu1)
  params$eta2 <- params$p_toicu2 * (params$mu_WC2)/(1- params$p_toicu2)
  params$eta3 <- params$p_toicu3 * (params$mu_WC3)/(1- params$p_toicu3)
  
  # not useful
  params$phi_I1 = 1/(params$p_phi*params$d_phi + (1-params$p_phi)*params$d_mu);
  params$phi_I2 = 1/(params$p_phi*params$d_phi + (1-params$p_phi)*params$d_mu);
  params$phi_I3 = 1/(params$p_phi*params$d_phi + (1-params$p_phi)*params$d_mu);
  
  ### Create vectors for inputs
  
  params$sigma_MS <- c(params$sigma_MS1,params$sigma_MS2,params$sigma_MS3) 
  params$sigma_C <- c(params$sigma_C1,params$sigma_C2,params$sigma_C3) 
  params$sigma_F <- c(params$sigma_F1,params$sigma_F2,params$sigma_F3) 
  
  params$mu_P <- c(params$mu_P1,params$mu_P2,params$mu_P3) 
  params$mu_MS <- c(params$mu_MS1,params$mu_MS2,params$mu_MS3) 
  params$mu_I <- c(params$mu_I1,params$mu_I2,params$mu_I3) 
  params$mu_WC <- c(params$mu_WC1,params$mu_WC2,params$mu_WC3) 
  params$mu_C <- c(params$mu_C1,params$mu_C2,params$mu_C3) 
  params$mu_WF <- c(params$mu_WF1,params$mu_WF2,params$mu_WF3) 
  params$mu_F <- c(params$mu_F1,params$mu_F2,params$mu_F3) 
  
  
  params$xi_MS <- c(params$xi_MS1,params$xi_MS2,params$xi_MS3) 
  
  params$theta_WF <- c(params$theta_WF1,params$theta_WF2,params$theta_WF3) 
  params$theta_F <- c(params$theta_F1,params$theta_F2,params$theta_F3) 
  
  params$eta <- c(params$eta1,params$eta2,params$eta3) 
  params$zeta <- c(params$zeta1,params$zeta2,params$zeta3) 
  
  params$chi_L <- c(params$chi_L1,params$chi_L2,params$chi_L3) 
  params$chi_LQ <- c(params$chi_LQ1,params$chi_LQ2,params$chi_LQ3) 
  params$chi_C <- c(params$chi_C1,params$chi_C2,params$chi_C3) 
  
  params$phi <- c(params$phi1,params$phi2,params$phi3) 
  params$phi_I <- c(params$phi_I1,params$phi_I2,params$phi_I3) 
  
  params$age <- c(params$young, params$medium, params$old)
  
  params
  
  
}
