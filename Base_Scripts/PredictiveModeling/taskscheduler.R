library(taskscheduleR)
taskscheduler_create('check10',
                     "C:\\Users\\reece\\Desktop\\R_Script_Tasks\\permeabilityNT.R", 
                     schedule = 'ONCE')
file.choose()
?taskscheduler_create
