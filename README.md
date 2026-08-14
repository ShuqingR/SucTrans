# File structure

SucTrans/  
  ├── code/  
  ├── data/  
  ├── equip/  
  ├── README.md  

# code  
code used for data analysis and visualisation.  

- count_glm.R
a script analysis of cb count data ~ sugar concentration. incl: glm, rough plot of glm line+raw data point, box plot

- data_process.R
a script storing code used for raw data cleaning & formatting, generates csv files based on the raw "dissection_data.csv".
    
- data_explore.R
for rough data exploration, may comes random not structured, includes some data processing not writen into files.
    
- status_process.R
mod ExDissection.csv for survival analysis, change dates records to days after experiments (0-13), add an event column (1 = death at the day, 0 = death didn't happen naturally), output ExStatus.csv
    
- status_coxPH.R
fit survival models.
    

# data  
data recorded from experiments and processed during analysis.  

- dissection_data.csv
raw data recorded during micoscopy check of crithidia from bee guts dissected (incl. pilot study and experimental data)

- ExDissection.csv
subsetted dissection data with experimental data only (group 4-7), removed pilot study data which had slightly different experimental settings 

- EndDayDissection.csv
subsetted experimental dissection data with only the ones dissected freshly alive at the end day of each group, so the same development time was ensured.

- ExStatus.csv
output of status_process.R, available for status analysis like Cox PH.

# equip
files related to equipment control and adjustment, modified based on works of Charlotte Fryday.  

- circuit_code.md
code for Arduino boards controlling servos of transmission trains.
        
- Print_dish-holder_new.stl
modified food plate for transmission trains.

# Date  
2026 Aug
# Author  
Shuqing Ren
