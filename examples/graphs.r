rm(list = ls()) # Clear all variables

path = '/home/erik'
#path = '/media/Mobsol'
source(paste(path,'/Dropbox/Analysis/Rcode/plotting.r', sep=""))

#debug(plot_network_graphs)
ntwk_metrics = c("Clustering","Closeness_Centrality","Degree","Betweenness_Centrality","Core_Number","Load_Centrality","Degree_Centrality","Isolates","Triangles")
clinical_metrics = c("PSQI", "HO", "BMI", "Beck", "BDI_II", "Munich", "IQ", "TimedIQ", "TimeToPerformIPTest", "EducationLevel", "ESS", "BAI_anxiety", "ADA", "Gender", "Age", "BDNF")
group_metrics = c("Gender", "ADA", "BDNF")

group = "Gender"
#group = "BDNF"
compare = "BDNF"
#compare = "Gender"
#node = "Brain-Stem"
node = "Right-Hippocampus"
#node_metric = "Degree"
#node_metric = "Clustering"
#node_metric = "Closeness_Centrality"
#node_metric = "Betweenness_Centrality"
#node_metric = "Core_Number"
#node_metric = "Load_Centrality"
#node_metric = "Degree_Centrality"
#node_metric = "Isolates"
node_metric = "Triangles"

extra_metric = "PSQI"
filename = 'BDNF_newtitles_pruned.csv'
#filename = 'BDNF_NBS_pruned.csv'
#filename = 'BDNF_NBS_pruned_inv.csv'
omit = FALSE

# dev.off(dev.list()[1:length(dev.list())])
#plot_network_graphs(filename, extra_metric, compare, ntwk_metrics, clinical_metrics)
#plot_extra_graphs(filename, extra_metric, compare, group_metrics, ntwk_metrics, clinical_metrics)
#plot_nodal_bargraph(filename, node, node_metric, compare, group_metrics, ntwk_metrics, clinical_metrics)
#plot_nodal_bargraph_bygroup(filename, node, node_metric, group, compare, group_metrics, ntwk_metrics, clinical_metrics)
#plot_network_metric_correlation(filename, ntwk_metrics)
#plot_clinical_metric_correlation(filename, ntwk_metrics, group_metrics, clinical_metrics, omit)
#plot_all_metric_correlation(filename, ntwk_metrics, group_metrics, clinical_metrics, omit)
#plot_average_bargraph_bygroup(filename, node_metric, group, compare, group_metrics, ntwk_metrics, clinical_metrics, node)
#compute_linear_models(filename, ntwk_metrics, group_metrics, clinical_metrics, omit)

#plot_average_bargraph(filename, node_metric, compare, group_metrics, ntwk_metrics, clinical_metrics, node)
#plot_total_edges_bargraph_bysubject(filename)
#plot_total_edges_histogram_bygroup(filename, compare)
#plot_average_edges_bargraph_bygroup(filename, compare)
#plot_metric_histogram_bygroup(filename, node_metric, compare)
#plot_average_metric_bargraph_bygroup(filename, node_metric, compare)

#plot_metric_histogram_bygroup(filename, extra_metric, compare)
plot_average_metric_bargraph_bygroup(filename, extra_metric, compare)



