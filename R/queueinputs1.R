#' @export
update_inputs <- function() {
  
  params = yaml.load_file( system.file("content/parameter_values1.yaml", package='covid19icu') )

  
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
  M_C1 = matrix(
    c(
      1-params$p_death_ICU1, - params$p_death_ICU1,
      params$avg_LOS_ICU1, params$avg_LOS_ICU1
    ),
   ncol=2            
  )
  
  out_C1 <- solve(M_C1, c(0,1))
  params$mu_C1 <- out_C1[1];
  params$chi_C1 <- out_C1[2];
  
  M_C2 = matrix(
    c(
      1-params$p_death_ICU2, - params$p_death_ICU2,
      params$avg_LOS_ICU2, params$avg_LOS_ICU2
    ),
    ncol=2            
  )
  
  out_C2 <- solve(M_C2, c(0,1))
  params$mu_C2 <- out_C2[1];
  params$chi_C2 <- out_C2[2];
  
  M_C3 = matrix(
    c(
      1-params$p_death_ICU3, - params$p_death_ICU3,
      params$avg_LOS_ICU3, params$avg_LOS_ICU3
    ),
    ncol=2            
  )
  
  out_C3 <- solve(M_C3, c(0,1))
  params$mu_C3 <- out_C3[1];
  params$chi_C3 <- out_C3[2];
  
  ######################
  # Leaving Floor
  M_F1 = matrix(
    c(
      1-params$p_death_Floor1, - params$p_death_Floor1, - params$p_death_Floor1,
      -params$p_stepup_Floor1, 1- params$p_stepup_Floor1, - params$p_stepup_Floor1,
      params$avg_LOS_Floor1, params$avg_LOS_Floor1, params$avg_LOS_Floor1
    ),
    ncol=3            
  )
  
  out_F1 <- solve(M_F1, c(0,0,1))
  params$mu_F1 <- out_F1[1];
  params$theta_F1 <- out_F1[2];
  params$chi_L1 <- out_F1[3];
  
  M_F2 = matrix(
    c(
      1-params$p_death_Floor2, - params$p_death_Floor2, - params$p_death_Floor2,
      -params$p_stepup_Floor2, 1- params$p_stepup_Floor2, - params$p_stepup_Floor2,
      params$avg_LOS_Floor2, params$avg_LOS_Floor2, params$avg_LOS_Floor2
    ),
    ncol=3            
  )
  
  out_F2 <- solve(M_F2, c(0,0,1))
  params$mu_F2 <- out_F2[1];
  params$theta_F2 <- out_F2[2];
  params$chi_L2 <- out_F2[3];
  
  
  M_F3 = matrix(
    c(
      1-params$p_death_Floor3, - params$p_death_Floor3, - params$p_death_Floor3,
      -params$p_stepup_Floor3, 1- params$p_stepup_Floor3, - params$p_stepup_Floor3,
      params$avg_LOS_Floor3, params$avg_LOS_Floor3, params$avg_LOS_Floor3
    ),
    ncol=3            
  )
  
  out_F3 <- solve(M_F3, c(0,0,1))
  params$mu_F3 <- out_F3[1];
  params$theta_F3 <- out_F3[2];
  params$chi_L3 <- out_F3[3];
  
  ######################
  # Leaving MS
  M_MS1 = matrix(
    c(
      1-params$p_death_MS1, - params$p_death_MS1, - params$p_death_MS1,
      -params$p_return_MS1, 1- params$p_return_MS1, - params$p_return_MS1,
      params$avg_T_exit1, params$avg_T_exit1, params$avg_T_exit1
    ),
    ncol=3            
  )
  
  out_MS1 <- solve(M_MS1, c(0,0,1))
  params$mu_MS1 <- out_MS1[1];
  params$xi_MS1 <- out_MS1[2];
  params$phi1 <- out_MS1[3];
  
  M_MS2 = matrix(
    c(
      1-params$p_death_MS2, - params$p_death_MS2, - params$p_death_MS2,
      -params$p_return_MS2, 1- params$p_return_MS2, - params$p_return_MS2,
      params$avg_T_exit2, params$avg_T_exit2, params$avg_T_exit2
    ),
    ncol=3            
  )
  
  out_MS2 <- solve(M_MS2, c(0,0,1))
  params$mu_MS2 <- out_MS2[1];
  params$xi_MS2 <- out_MS2[2];
  params$phi2 <- out_MS2[3];
  
  
  M_MS3 = matrix(
    c(
      1-params$p_death_MS3, - params$p_death_MS3, - params$p_death_MS3,
      -params$p_return_MS3, 1- params$p_return_MS3, - params$p_return_MS3,
      params$avg_T_exit3, params$avg_T_exit3, params$avg_T_exit3
    ),
    ncol=3            
  )
  
  out_MS3 <- solve(M_MS3, c(0,0,1))
  params$mu_MS3 <- out_MS3[1];
  params$xi_MS3 <- out_MS3[2];
  params$phi3 <- out_MS3[3];
  
  ######################
  # Leaving FL Queue
  M_WF1 = matrix(
    c(
      1-params$p_death_WF1, - params$p_death_WF1, - params$p_death_WF1,
      -params$p_stepup_WF1, 1- params$p_stepup_WF1, - params$p_stepup_WF1,
      params$avg_LOS_FloorQ1, params$avg_LOS_FloorQ1, params$avg_LOS_FloorQ1
    ),
    ncol=3            
  )
  
  out_WF1 <- solve(M_WF1, c(0,0,1))
  params$mu_WF1 <- out_WF1[1];
  params$theta_WF1 <- out_WF1[2];
  params$chi_LQ1 <- out_WF1[3];
  
  M_WF2 = matrix(
    c(
      1-params$p_death_WF2, - params$p_death_WF2, - params$p_death_WF2,
      -params$p_stepup_WF2, 1- params$p_stepup_WF2, - params$p_stepup_WF2,
      params$avg_LOS_FloorQ2, params$avg_LOS_FloorQ2, params$avg_LOS_FloorQ2
    ),
    ncol=3            
  )
  
  out_WF2 <- solve(M_WF2, c(0,0,1))
  params$mu_WF2 <- out_WF2[1];
  params$theta_WF2 <- out_WF2[2];
  params$chi_LQ2 <- out_WF2[3];
  
  M_WF3 = matrix(
    c(
      1-params$p_death_WF3, - params$p_death_WF3, - params$p_death_WF3,
      -params$p_stepup_WF3, 1- params$p_stepup_WF3, - params$p_stepup_WF3,
      params$avg_LOS_FloorQ3, params$avg_LOS_FloorQ3, params$avg_LOS_FloorQ3
    ),
    ncol=3            
  )
  
  out_WF3 <- solve(M_WF3, c(0,0,1))
  params$mu_WF3 <- out_WF3[1];
  params$theta_WF3 <- out_WF3[2];
  params$chi_LQ3 <- out_WF3[3];
  
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
  
  params
  
  
}
