### Model structure

**This model captures some of the most important ways COVID19+ patients move through a hospital.**

COVID19+ patients enter the hospital through the emergency department (ED), where they are triaged to the ICU, the general medicine
floor, or discharge as necessary.  If bed capacity is reached, these patients enter floor or ICU queues until a bed opens.  Patients 
move between the queues and between the floor and ICU depending on their status.  Deaths may occur while waiting in queues, on the floor,
in the ICU, and post-discharge from the ED.  

<img src="Modelstructure1.png" width="900">

**Inputs on the "Scenario" tab allow you to specify a local infection scenario.** 

Changing these inputs results in changes in the number of cases presenting to the ED daily (Plot A) and the total number of ED visits (Plot B).

**Time horizon**: the number of days for which you wish to see projections.

**Infection curve**: the curve which describes how infections will increase. 

**Initial cases per day**: the number of COVID+ patients presenting daily at the beginning of the simulation .

**Doubling time (exponential infection curve)**: if cases are increasing exponentially, the number of days it will take for the number of cases presenting to the ED daily to double.

**Rate of increase in new cases per day (linear infection curve)**: 


