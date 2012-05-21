rm(list = ls()) # Clear all variables

path = '/home/erik'
#path = '/media/Mobsol'
source(paste(path,'/Dropbox/Analysis/Rcode/plotting.r', sep=""))
source(paste(path,'/Dropbox/Analysis/Rcode/stats.r', sep=""))

metrics = c("endpoint_n_fib","intersections_n_fib","endpoints_percent","intersections_percent","orig_n_fib")
clinical_metrics = c("PSQI", "HO", "BMI", "Beck", "BDI_II", "Munich", "IQ", "TimedIQ", "TimeToPerformIPTest", "EducationLevel", "ESS", "BAI_anxiety", "ADA", "Gender", "Age", "BDNF")
group_metrics = c("Gender", "ADA", "BDNF")

group = "Gender"
#group = "BDNF"
compare = "BDNF"
#compare = "Gender"
#node_metric = "Degree"
metric = "intersections_percent"
extra_metric = "PSQI"
filename = 'fibers_newtitles.csv'
omit = FALSE

plot_fiber_bargraph(filename, metric, compare, group_metrics)
plot_fiber_bargraph_bygroup(filename, metric, group, compare, group_metrics, clinical_metrics)
