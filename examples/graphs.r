rm(list = ls()) # Clear all variables

path = '/Users/erik/Dropbox/Analysis/network-statplots/'

source(paste(path,'plotting.r', sep=""))
source(paste(path,'correlation.r', sep=""))
source(paste(path,'stats.r', sep=""))

#debug(plot_network_graphs)
ntwk_metrics = c("clustering","closeness_centrality","degree","betweenness_centrality","core_number","load_centrality","degree_centrality","isolates","triangles")
clinical_metrics = c("Subject","PSQI","HO","BMI","Beck","BDI_II","Munich","IQ","TimedIQ","Age","BDNF","ADA","Gender","TimeToPerformIPTest","education level","ESS","BAI_anxiety")
group_metrics = c("Gender", "ADA", "BDNF")

group = "Gender"
#group = "BDNF"
compare = "BDNF"
#compare = "Gender"
#node = "Brain-Stem"
node = "Right-Hippocampus"
node_metric = "degree"


extra_metric = "PSQI"
filename = '../data/BDNF.csv'
omit = FALSE

# dev.off(dev.list()[1:length(dev.list())])
#plot_network_graphs(filename, extra_metric, compare, ntwk_metrics, clinical_metrics)
#plot_extra_graphs(filename, extra_metric, compare, group_metrics, ntwk_metrics, clinical_metrics)
#plot_nodal_bargraph(filename, node, node_metric, compare, group_metrics, ntwk_metrics, clinical_metrics)
#plot_nodal_bargraph_bygroup(filename, node, node_metric, group, compare, group_metrics, ntwk_metrics, clinical_metrics)
#plot_network_metric_correlation(filename, ntwk_metrics)
#plot_clinical_metric_correlation(filename, ntwk_metrics, group_metrics, clinical_metrics, omit)
plot_all_metric_correlation(filename, ntwk_metrics, group_metrics, clinical_metrics, omit)
#plot_average_bargraph_bygroup(filename, node_metric, group, compare, group_metrics, ntwk_metrics, clinical_metrics, node)
#compute_linear_models(filename, ntwk_metrics, group_metrics, clinical_metrics, omit)

#plot_average_bargraph(filename, node_metric, compare, group_metrics, ntwk_metrics, clinical_metrics, node)
#plot_total_edges_bargraph_bysubject(filename)
#plot_total_edges_histogram_bygroup(filename, compare)
#plot_average_edges_bargraph_bygroup(filename, compare)
#plot_metric_histogram_bygroup(filename, node_metric, compare)
#plot_average_metric_bargraph_bygroup(filename, node_metric, compare)

#plot_metric_histogram_bygroup(filename, extra_metric, compare)
#plot_average_metric_bargraph_bygroup(filename, extra_metric, compare)



