#' @export
update_inputs <- function() {
  
  params = yaml.load_file( system.file("content/parameter_values.yaml", package='covid19icu') )

  
  ###solving set of linear equations for rate of stepping up from floor to ICU and
  ###death rate from floor
  
  M11 = matrix(0, 2,2)
  M11[1,1] = 1-params$ptheta_F1
  M11[1,2] = -params$ptheta_F1
  M11[2,1] = -params$pmu_F1
  M11[2,2] = 1-params$pmu_F1
  
  out11 = solve(M11)%*% matrix(params$chi_L1*c(params$ptheta_F1, params$pmu_F1), 2,1)
  theta_F1 = out11[1];
  mu_WF1 = out11[2]
  
  
  M21 = matrix(0, 2,2)
  M21[1,1] = 1-params$ptheta_F2
  M21[1,2] = -params$ptheta_F2
  M21[2,1] = -params$pmu_F2
  M21[2,2] = 1-params$pmu_F2
  
  out21 = solve(M21)%*% matrix(params$chi_L2*c(params$ptheta_F2, params$pmu_F2), 2,1)
  theta_F2 = out21[1];
  mu_WF2 = out21[2]
  
  M31 = matrix(0, 2,2)
  M31[1,1] = 1-params$ptheta_F3
  M31[1,2] = -params$ptheta_F3
  M31[2,1] = -params$pmu_F3
  M31[2,2] = 1-params$pmu_F3
  
  out31 = solve(M31)%*% matrix(params$chi_L3*c(params$ptheta_F3, params$pmu_F3), 2,1)
  theta_F3 = out31[1];
  mu_WF3 = out31[2]

  ###solving set of linear equations for rate of returning to ED and 
  ###death rate of patients triaged as having mild symptoms
  M12 = matrix(0,2,2)
  M12[1,1] = 1-params$pxi_MS1
  M12[1,2] = -params$pxi_MS1
  M12[2,1] = -params$pmu_MS1
  M12[2,2] = 1-params$pmu_MS1
  
  out12 = solve(M12)%*% matrix(params$phi1*c(params$pxi_MS1, params$pmu_MS1), 2,1)
  xi_MS1 = out12[1];
  mu_MS1 = out12[2];
  
  M22 = matrix(0,2,2)
  M22[1,1] = 1-params$pxi_MS2
  M22[1,2] = -params$pxi_MS2
  M22[2,1] = -params$pmu_MS2
  M22[2,2] = 1-params$pmu_MS2
  
  out22 = solve(M22)%*% matrix(params$phi2*c(params$pxi_MS2, params$pmu_MS2), 2,1)
  xi_MS2 = out22[1];
  mu_MS2 = out22[2];

  M32 = matrix(0,2,2)
  M32[1,1] = 1-params$pxi_MS3
  M32[1,2] = -params$pxi_MS3
  M32[2,1] = -params$pmu_MS3
  M32[2,2] = 1-params$pmu_MS3
  
  out32 = solve(M32)%*% matrix(params$phi3*c(params$pxi_MS3, params$pmu_MS3), 2,1)
  xi_MS3 = out32[1];
  mu_MS3 = out32[2];
  
  # Readmission rate to the ICU from floor queue

  #phi_I1 = 1/(params$p_phi1*params$d_phi1 + (1-params$p_phi1)*params$d_mu1);
  #lambda1 = (params$phi_I1 + params1$mu_I1)*params$p_lambda1/(1-params$p_lambda1)
  #phi_I2 = 1/(params$p_phi2*params$d_phi2 + (1-params$p_phi2)*params$d_mu2);
  #lambda2 = (params$phi_I2 + params1$mu_I2)*params$p_lambda2/(1-params$p_lambda2)
  #phi_I3 = 1/(params$p_phi3*params$d_phi3 + (1-params$p_phi3)*params$d_mu3);
  #lambda3 = (params$phi_I3 + params1$mu_I3)*params$p_lambda3/(1-params$p_lambda3)
  
  params$theta_F1 =theta_F1;
  params$mu_WF1 =mu_WF1; 
  params$theta_F2 =theta_F2;
  params$mu_WF2 =mu_WF2; 
  params$theta_F3 =theta_F3;
  params$mu_WF3 =mu_WF3; 
  
  # step up rate is undifferentiated between floor and floor queue
  params$theta_WF1 = params$ptheta_WF1*params$mu_WF1/(1-params$ptheta_WF1)
  params$theta_WF2 = params$ptheta_WF2*params$mu_WF2/(1-params$ptheta_WF2)
  params$theta_WF3 = params$ptheta_WF3*params$mu_WF3/(1-params$ptheta_WF3)
  
  params$theta_F1 = params$theta_F1
  params$theta_F2 = params$theta_F2
  params$theta_F3 = params$theta_F3
  
  # Death rate is also undifferentiated
  params$mu_F1 = mu_WF1
  params$mu_F2 = mu_WF2
  params$mu_F3 = mu_WF3
  
  
  
  
  params$xi_MS1 =xi_MS1;
  params$mu_MS1 =mu_MS1; 
  params$xi_MS2 =xi_MS2;
  params$mu_MS2 =mu_MS2; 
  params$xi_MS3 =xi_MS3;
  params$mu_MS3 =mu_MS3; 
  
  
  
  params$sigma_MS1= params$percentage_MS1 * 1/params$t_Triage;
  params$sigma_MS2= params$percentage_MS2 * 1/params$t_Triage;
  params$sigma_MS3= params$percentage_MS3 * 1/params$t_Triage;
  params$sigma_F1= params$percentage_F1 * 1/params$t_Triage;
  params$sigma_F2= params$percentage_F2 * 1/params$t_Triage;
  params$sigma_F3= params$percentage_F3 * 1/params$t_Triage;
  params$sigma_C1= params$percentage_C1 * 1/params$t_Triage;
  params$sigma_C2= params$percentage_C2 * 1/params$t_Triage;
  params$sigma_C3= params$percentage_C3 * 1/params$t_Triage;
  
  
  params$phi_I1 = 1/(params$p_phi*params$d_phi + (1-params$p_phi)*params$d_mu);
  params$phi_I2 = 1/(params$p_phi*params$d_phi + (1-params$p_phi)*params$d_mu);
  params$phi_I3 = 1/(params$p_phi*params$d_phi + (1-params$p_phi)*params$d_mu);
  
  params
  
  
}
