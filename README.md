# FRIENDanalysis
This package is designed to assist with projects related to FRIEND (the Fitness Registry and Importance of Exercise International Database). 

### Compiling data for a contribution to FRIEND.
FRIEND is a database of cardiopulmonary exercise tests (VO2max tests) from around the world. 
To assist labs with compiling their data so that they can contribute to FRIEND, I created various functions.
The key for these functions was to create something that could be used by those unfamiliar with R.
Step-by-step instructions can be provided to those interested (please reach out!).

The first set of functions allow a user to compile data from a Parvo metabolic cart.
(functionality with a Cosmed metabolic cart is in the works)
The user can then verify the data is correct (error messages are displayed at the top of the resulting Excel file).

Functions involved with compiling the data include: 
1) folder_location()
2) read_Parvo_data()
3) read_Cosmed_data()
4) save_data()
The folder_location() and save_data() functions provide an easy to use graphical user interface.
The read_Parvo_data() or read_Cosmed_data() functions compile the data (and which you use is dependent on which metabolic cart was used to collect the data).


### Calculating FRIEND VO2max percentiles.
FRIEND has published reference standards to assist with interpreting VO2max values.
(Kaminsky et al. MCP 2015, Kaminsky et al. MCP 2017, Kaminsky et al. MCP 2021)
These reference standards are based on age, sex, and test mode (treadmill vs cycling).

I created a function to calculate an individual's VO2max percentile: FRIENDpercentile().
This function takes an individual's age, sex, testing mode, and measured VO2max to determine their percentile.
With the recent update to the reference standards (in 2021), one can now decide which edition of the reference standards they would like to use.

Step-by-step instructions for those unfamiliar with R can be provided if interested (again, please reach out!).


### Calculating FRIEND ventilatory threshold (VT) percentiles.
FRIEND has published reference standards to assist with interpreting measured VT values.
(Vainshelboim et al. Chest 2019)
These reference standards are based on age, sex, and test mode (treadmill vs cycling).

I created a function to calculate an individual's VT percentile: VT_FRIENDpercentile().
This function takes an individual's age, sex, testing mode, and measured VT to determine their percentile.
