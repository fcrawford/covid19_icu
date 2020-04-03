**<h3> MODEL OUTPUTS </h3>**

<br/>

<h4> **Notice:** Model outputs are completely dependent on user-defined inputs and depend on assumptions encoded in the model structure. 
Please familiarize yourself with the model structure and inputs for accurate interpretation of these outputs. <h4/>

<br/> 

#### **Plot A** 

This plot depicts the **COVID19+ cases presenting to the health system per day** during the days included in the simulation.  For example, on day 10, the plot 	gives the number of COVID19+ cases which presented to the ED on day 10.  This is the input to the simulation specified in the scenario.

<br/> 

#### **Plot B**

This plot depicts **cumulative presentations of COVID19+ patients to the health system and cumulative deaths of COVID19+ patients**.

* **Cumulative presentations of COVID19+ patients to the health system (black curve)**: the total number of ED visits which occured in the simulation since time 0.  For example,
	on day 10, the black curve gives the total number of ED visits that occurred between day 0 and day 10.

* **Cumulative deaths of COVID19+ patients (red curve)**: the total number of deaths observed in the simulation since time 0.  For example, on day 10, the red curve
	gives the total number of deaths that occurred between day 0 and day 10. 

<br/> 
 
#### **Plot C** 

This plot depicts **cumulative deaths of COVID19+ patients by location**, the total number of deaths observed from each compartment in the model. On a given day, the difference between the minimum
	and maximum values spanned by an area of a particular color represents the number of total deaths which occurred in that compartment
	between time 0 and that day.

<br/> 

#### **Plot D**

This plot depicts **ICU and floor utilization by COVID19+ patients** on a given day. 

* **Maximum ICU capacity for COVID19+ patients (black dashed):** the maximum ICU capacity of your health system which is available to COVID19+ patients. A tag appears with the day on which capacity is exceeded when the number of COVID19+ patients requiring ICU care in the simulation
	exceeds the maximum ICU capacity available to COVID19+ patients.  Maximum capacity changes depending on the capacity expansion specified by the user.

* **In ICU (black):** the number of COVID19+ patients who are in the ICU on a given day.

* **Waiting for ICU beds (gray):** the number of COVID19+ patients who are in the ICU queue on a given day.

* **On floor (red):** the number of COVID19+ patients who are on the floor on a given day.

* **Maximum floor capacity (red dashed):** the maximum floor capacity of your health system available to COVID19+ patients. A tag appears with the day on which capacity is exceeded when the number of COVID19+ patients requiring floor beds in the simulation
	exceeds the maximum floor capacity.  Maximum capacity changes depending on the capacity expansion specified by the user.

* **Waiting for floor beds (pink):** the number of COVID19+ patients who are in the floor queue on a given day.

<br/> 

#### **Summary of model outputs**

* **Case fatality ratio:** the total number of deaths of COVID19+ patinets occuring in the simulations divided by the sum of the initial number of COVID19+ patients in the hospital and the number of COVID19+ presentations to the health system, expressed as a percentage.

* **Number of ICU beds needed:** the number of ICU beds needed for COVID19+ patients to ensure that there will never be a queue for ICU beds.

* **Number of floor beds needed:** the number of floor beds needed for COVID19+ patients to ensure that there will never be a queue for floor beds.