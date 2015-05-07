rm(list=ls())

library(RODBCext)
library(ggplot2)
library(dplyr)
library(plyr)
library(moments)

buildings_per_sector = function(sec) {
	query=readLines("buildings_per_sector.sql")
	query=paste(query, collapse=" \n ")
	query=gsub("XXXX", sec, query)
	buildings_per_sector=sqlExecute(conn, query, NULL, fetch=T)
	buildings_per_sector$sector_name=as.factor(buildings_per_sector$sector_name)
	buildings_per_sector$sector_name=gsub(" ", "\n", buildings_per_sector$sector_name)
	title=paste("Buildings per sector ", sec)
	filename=paste0("figures/buildings_per_sector_", sec, ".png")
	ggplot(buildings_per_sector) + 
	geom_histogram(aes(x=sector_name, y=count), stat="identity") +
	xlab("subsector name") +
	ylab("count") +
	ggtitle(title)
	ggsave(file=filename)
}

buildings_per_sector_mapping = function(sec) {
	query=readLines("buildings_per_sector_mapping.sql")
	query=paste(query, collapse=" \n ")
	query=gsub("XXXX", sec, query)
	mapping=sqlExecute(conn, query, NULL, fetch=T)
	levels(mapping$sector_name) = c(levels(mapping$sector_name), "NA")
	mapping$sector_name[mapping$sector_name=='']="NA"
	mapping$sector_name=as.factor(mapping$sector_name)
	mapping$sector_name=gsub(" ", "\n", mapping$sector_name)
	title=paste("Buildings per sector", sec)
	filename=paste0("figures/buildings_per_sector_", sec, "_mapping.png")
	ggplot(mapping) +
	geom_histogram(aes(x=sector_name, y=count, fill=mapping), stat='identity') +
	ggtitle(title)
	ggsave(file=filename)
}

plot_1_n_mapping = function(protocol, sector_id, sector_description) {
	query=readLines("1_n_mapping.sql")
	query=paste(query, collapse=" \n ")
	query=gsub("XXXX", protocol, query)
	parameters=data.frame(a=sector_id)
	mappings=sqlExecute(conn, query, parameters, fetch=T)
	mappings$func=as.factor(mappings$func)
	title = paste0("Building functions of sector '", sector_description, "' (", protocol, ") with 1:n mapping")
	filename=paste0("figures/", sector_description, "_1_n_", protocol, ".png")
	ggplot(mappings) +
	geom_histogram(aes(x=func, y=count), stat='identity') + 
	ggtitle(title)
	ggsave(file=filename)
}

plot_co2_emissions = function(protocol) {
	query=readLines("co2.sql")
	query=paste(query, collapse=" \n ")
	query=gsub("XXXX", protocol, query)
	co2=sqlExecute(conn, query, NULL, fetch=T)
	levels(co2$sector_name) = c(levels(co2$sector_name), "NA")
	co2$sector_name[co2$sector_name=='']="NA"
	co2$sector_name=as.factor(co2$sector_name)
	co2$sector_name=gsub(" ", "\n", co2$sector_name)
	title=paste("CO2 per sector", protocol)
	filename=paste0("figures/co2_", protocol, ".png")
	ggplot(co2) +
		geom_histogram(aes(x=sector_name, y=co2), stat="identity") + 
		xlab("Sectors") +
		ylab("CO2 [kg/a]") +
		ggtitle(title)
	ggsave(filename)
}

conn=odbcConnect("local", uid="richard", case="postgresql")

# GPC statistics
buildings_per_sector("gpc")
buildings_per_sector_mapping("gpc")
plot_1_n_mapping("gpc", 1, "residential_buildings")

# ccr statistics
buildings_per_sector("ccr")

# ecoregion statistics
buildings_per_sector_mapping("ecoregion")
plot_1_n_mapping("ecoregion", 4, "governmental_buildings")

# CO2 emissions per protocol per sector
plot_co2_emissions("gpc")
plot_co2_emissions("ccr")
plot_co2_emissions("ecoregion")

# Boxplots per protocol per sector
query=readLines("co2_all.sql")
query=paste(query, collapse=" \n ")
query=gsub("XXXX", "gpc", query)
co2_gpc_all=sqlExecute(conn, query, NULL, fetch=T)
co2_gpc_all$sector_name=gsub(" ", "\n", co2_gpc_all$sector_name)
co2_gpc_all$sector_name=as.factor(co2_gpc_all$sector_name)
stats = boxplot.stats(co2_gpc_all$co2)$stats
iqr = IQR(co2_gpc_all$co2)
ggplot(co2_gpc_all, aes(x=sector_name, y=co2)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Subsectors") +
	ylab("CO2 [kg/a]") +
	ggtitle("CO2 per subsector GPC.")
ggsave(file="figures/boxplot_gpc.png")

stats = boxplot.stats(co2_gpc_all$ratio_vol)$stats
iqr = IQR(co2_gpc_all$ratio_vol)
ggplot(co2_gpc_all, aes(x=sector_name, y=ratio_vol)) +
	geom_boxplot() + 
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Subsectors") +
	ylab("CO2 [kg/m³a Vol]") +
	ggtitle("CO2 per subsector GPC normalized by building volume.")
ggsave(file="figures/boxplot_gpc_norm_vol.png")

stats = boxplot.stats(co2_gpc_all$ratio_ground)$stats
iqr = IQR(co2_gpc_all$ratio_ground)
ggplot(co2_gpc_all, aes(x=sector_name, y=ratio_ground)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Subsectors") +
	ylab("CO2 [kg/m²a GroundArea]") +
	ggtitle("CO2 per subsector GPC normalized by building's groundsurface.")
ggsave(file="figures/boxplot_gpc_norm_ground.png")

counts = ddply(.data=co2_gpc_all, .(sector_name), summarize, n=paste0("n=", length(sector_name)))
ggplot(co2_gpc_all, aes(x=co2)) +
	geom_density() +
	facet_wrap(~ sector_name) + 
	geom_text(data=counts, aes(x=1000, y=.15, label=n), colour="black", inherit.aes=F, parse=F) +
	ggtitle("Probability density GPC, normalized by building's groundsurface.")
ggsave(file="figures/pdf_gpc_norm_ground.png")

# Boxplot ccr
query=readLines("co2_all.sql")
query=paste(query, collapse=" \n ")
query=gsub("XXXX", "ccr", query)
co2_ccr_all=sqlExecute(conn, query, NULL, fetch=T)
co2_ccr_all$subsector_name=as.factor(co2_ccr_all$sector_name)
co2_ccr_all$subsector_name=gsub(" ", "\n", co2_ccr_all$sector_name)
stats = boxplot.stats(co2_ccr_all$co2)$stats
iqr = IQR(co2_ccr_all$co2)

ggplot(co2_ccr_all, aes(x=sector_name, y=co2)) +
	geom_boxplot() + 
	stat_summary(fun.y=mean, geom="point",shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/a]") +
	ggtitle("CO2 per sector CCR.")
ggsave(file="figures/boxplot_ccr.png")

stats = boxplot.stats(co2_ccr_all$ratio_vol)$stats
iqr = IQR(co2_ccr_all$ratio_vol)
ggplot(co2_ccr_all, aes(x=sector_name, y=ratio_vol)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m³a Vol]") +
	ggtitle("CO2 per sector CCR normalized by building volume.")
ggsave(file="figures/boxplot_ccr_norm_vol.png")

stats = boxplot.stats(co2_ccr_all$ratio_ground)$stats
iqr = IQR(co2_ccr_all$ratio_ground)
ggplot(co2_ccr_all, aes(x=sector_name, y=ratio_ground)) +
	geom_boxplot() + 
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m²a GroundArea]") +
	ggtitle("CO2 per sector CCR normalized by building's groundsurface.")
ggsave(file="figures/boxplot_ccr_norm_ground.png")

# Boxplot ecoregion
query=readLines("co2_all.sql")
query=paste(query, collapse=" \n ")
query=gsub("XXXX", "ecoregion", query)
co2_eco_all=sqlExecute(conn, query, NULL, fetch=T)
levels(co2_eco_all$sector_name) = c(levels(co2_eco_all$sector_name), "NA")
co2_eco_all$sector_name[co2_eco_all$sector_name=='']='NA'
co2_eco_all$sector_name=as.factor(co2_eco_all$sector_name)
co2_eco_all$sector_name=gsub(" ", "\n", co2_eco_all$sector_name)
stats = boxplot.stats(co2_eco_all$co2)$stats
iqr = IQR(co2_eco_all$co2)
ggplot(co2_eco_all, aes(x=sector_name, y=co2)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/a]") +
ggtitle("CO2 per sector EcoRegion.")
ggsave(file="figures/boxplot_eco.png")

stats = boxplot.stats(co2_eco_all$ratio_vol)$stats
iqr = IQR(co2_eco_all$ratio_vol)
ggplot(co2_eco_all, aes(x=sector_name, y=ratio_vol)) +
	geom_boxplot() + 
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m³a Vol]") +
	ggtitle("CO2 per sector EcoRegion normalized by building volume.")
ggsave(file="figures/boxplot_eco_norm_vol.png")

stats = boxplot.stats(co2_eco_all$ratio_ground)$stats
iqr = IQR(co2_eco_all$ratio_ground)
ggplot(co2_eco_all, aes(x=sector_name, y=ratio_ground)) +
	geom_boxplot() + 
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m²a GroundArea]") +
	ggtitle("CO2 per sector EcoRegion normalized by building's groundsurface.")
ggsave(file="figures/boxplot_eco_norm_ground.png")

counts = ddply(.data=co2_eco_all, .(sector_name), summarize, n=paste0("n=", length(sector_name)))
ggplot(co2_eco_all, aes(x=co2)) +
	geom_density() +
	facet_wrap(~ sector_name) + 
	geom_text(data=counts, aes(x=250, y=.15, label=n), colour="black", inherit.aes=F, parse=F) +
	ggtitle("Probability density EcoRegion, normalized by building's groundsurface.")
ggsave(file="figures/pdf_eco_norm_ground.png")

########################################################################################
#
# Boxplots of residential buildings
#
########################################################################################
residential_gpc = filter(co2_gpc_all, sector_name=="Residential\nbuildings")[, 2:7]
colnames(residential_gpc)[1] = "sector_name"
residential_gpc$sector_name="Residential\nbuildings\nGPC"

residential_ccr = filter(co2_ccr_all, subsector_name=="Residential")[, 2:7]
residential_ccr$sector_name = "Residential\nbuildings\nccR"

residential_eco = filter(co2_eco_all, sector_name=="Haushalte")[, 2:7]
residential_eco$sector_name = "Residential\nbuildings\nEcoRegion"
residential_eco$subsector_id = NULL
residential_eco$subsector_name = NULL

residential = residential_gpc
residential = rbind(residential, residential_ccr)
residential = rbind(residential, residential_eco)

stats = boxplot.stats(residential$co2)$stats
iqr = IQR(residential$co2)
skew_gpc = round(skewness(residential_gpc$co2), 2)
skew_ccr = round(skewness(residential_ccr$co2), 2)
skew_eco = round(skewness(residential_eco$co2), 2)
ggplot(residential, aes(x=sector_name, y=co2)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=1, y=7, label=skew_ccr) +
	geom_text(data=NULL, x=3, y=7, label=skew_gpc) +
	geom_text(data=NULL, x=2, y=7, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/a]") +
	ggtitle("CO2 per sector.")
ggsave(file="figures/boxplot_residential.png")

stats = boxplot.stats(residential$ratio_vol)$stats
iqr = IQR(residential$ratio_vol)
skew_gpc = round(skewness(residential_gpc$ratio_vol), 2)
skew_ccr = round(skewness(residential_ccr$ratio_vol), 2)
skew_eco = round(skewness(residential_eco$ratio_vol), 2)
ggplot(residential, aes(x=sector_name, y=ratio_vol)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=1, y=11, label=skew_ccr) +
	geom_text(data=NULL, x=3, y=11, label=skew_gpc) +
	geom_text(data=NULL, x=2, y=11, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m³a Vol]") +
	ggtitle("CO2 per sector normalized by building volume.")
ggsave(file="figures/boxplot_residential_norm_vol.png")

stats = boxplot.stats(residential$ratio_ground)$stats
iqr = IQR(residential$ratio_ground)
skew_gpc = round(skewness(residential_gpc$ratio_ground), 2)
skew_ccr = round(skewness(residential_ccr$ratio_ground), 2)
skew_eco = round(skewness(residential_eco$ratio_ground), 2)
ggplot(residential, aes(x=sector_name, y=ratio_ground)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=1, y=70, label=skew_ccr) +
	geom_text(data=NULL, x=3, y=70, label=skew_gpc) +
	geom_text(data=NULL, x=2, y=70, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m²a GroundArea]") +
	ggtitle("CO2 per sector normalized by building's ground surface.")
ggsave(file="figures/boxplot_residential_norm_ground.png")

########################################################################################
#
# Boxplots of commercial buildings
#
########################################################################################
commercial_gpc = filter(co2_gpc_all, sector_name=="Commercial\nand\ninstitutional\nbuildings/facilites")[, 2:7]
colnames(commercial_gpc)[1] = "sector_name"
commercial_gpc$sector_name="Commercial\nand\ninstitutional\nbuildings/facilites\nGPC"

commercial_ccr = filter(co2_ccr_all, subsector_name=="Commercial")[, 2:7]
commercial_ccr$sector_name = "Commercial\nccR"

commercial_eco = filter(co2_eco_all, sector_name=="Tertiärer\nSektor:\nWirtschaft")[, 2:7]
commercial_eco$sector_name = "Tertiärer Sektor\nEcoRegion"
commercial_eco$subsector_id = NULL
commercial_eco$subsector_name = NULL

commercial = commercial_gpc
commercial = rbind(commercial, commercial_ccr)
commercial = rbind(commercial, commercial_eco)

stats = boxplot.stats(commercial$co2)$stats
iqr = IQR(commercial$co2)
skew_gpc = round(skewness(commercial_gpc$co2), 2)
skew_ccr = round(skewness(commercial_ccr$co2), 2)
skew_eco = round(skewness(commercial_eco$co2), 2)
ggplot(commercial, aes(x=sector_name, y=co2)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=2, y=15, label=skew_ccr) +
	geom_text(data=NULL, x=1, y=15, label=skew_gpc) +
	geom_text(data=NULL, x=3, y=15, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/a]") +
	ggtitle("CO2 per sector.")
ggsave(file="figures/boxplot_commercial.png")

stats = boxplot.stats(commercial$ratio_vol)$stats
iqr = IQR(commercial$ratio_vol)
skew_gpc = round(skewness(commercial_gpc$ratio_vol), 2)
skew_ccr = round(skewness(commercial_ccr$ratio_vol), 2)
skew_eco = round(skewness(commercial_eco$ratio_vol), 2)
ggplot(commercial, aes(x=sector_name, y=ratio_vol)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=2, y=6, label=skew_ccr) +
	geom_text(data=NULL, x=1, y=6, label=skew_gpc) +
	geom_text(data=NULL, x=3, y=6, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m³a Vol]") +
	ggtitle("CO2 per sector normalized by building volume.")
ggsave(file="figures/boxplot_commercial_norm_vol.png")

stats = boxplot.stats(commercial$ratio_ground)$stats
iqr = IQR(commercial$ratio_ground)
skew_gpc = round(skewness(commercial_gpc$ratio_ground), 2)
skew_ccr = round(skewness(commercial_ccr$ratio_ground), 2)
skew_eco = round(skewness(commercial_eco$ratio_ground), 2)
ggplot(commercial, aes(x=sector_name, y=ratio_ground)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=2, y=25, label=skew_ccr) +
	geom_text(data=NULL, x=1, y=25, label=skew_gpc) +
	geom_text(data=NULL, x=3, y=25, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m²a GroundArea]") +
	ggtitle("CO2 per sector normalized by building's ground surface.")
ggsave(file="figures/boxplot_commercial_norm_ground.png")

########################################################################################
#
# Boxplots of industrial buildings
#
########################################################################################
industrial_gpc = filter(co2_gpc_all, sector_name=="Manufactoring\nindustry\nand\nconstruction")[, 2:7]
colnames(industrial_gpc)[1] = "sector_name"
industrial_gpc$sector_name="Manufactoring\nindustry\nand\nconstruction\nGPC"

industrial_ccr = filter(co2_ccr_all, subsector_name=="Industrial")[, 2:7]
industrial_ccr$sector_name = "Industrial\nccR"

industrial_eco = filter(co2_eco_all, sector_name=="Sekundärer\nSektor:\nWirtschaft")[, 2:7]
industrial_eco$sector_name = "Sekundärer Sektor\nEcoRegion"
industrial_eco$subsector_id = NULL
industrial_eco$subsector_name = NULL

industrial = industrial_gpc
industrial = rbind(industrial, industrial_ccr)
industrial = rbind(industrial, industrial_eco)

stats = boxplot.stats(industrial$co2)$stats
iqr = IQR(industrial$co2)
skew_gpc = round(skewness(industrial_gpc$co2), 2)
skew_ccr = round(skewness(industrial_ccr$co2), 2)
skew_eco = round(skewness(industrial_eco$co2), 2)
ggplot(industrial, aes(x=sector_name, y=co2)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=1, y=15, label=skew_ccr) +
	geom_text(data=NULL, x=2, y=15, label=skew_gpc) +
	geom_text(data=NULL, x=3, y=15, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/a]") +
	ggtitle("CO2 per sector.")
ggsave(file="figures/boxplot_industrial.png")

stats = boxplot.stats(industrial$ratio_vol)$stats
iqr = IQR(industrial$ratio_vol)
skew_gpc = round(skewness(industrial_gpc$ratio_vol), 2)
skew_ccr = round(skewness(industrial_ccr$ratio_vol), 2)
skew_eco = round(skewness(industrial_eco$ratio_vol), 2)
ggplot(industrial, aes(x=sector_name, y=ratio_vol)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=1, y=10, label=skew_ccr) +
	geom_text(data=NULL, x=2, y=10, label=skew_gpc) +
	geom_text(data=NULL, x=3, y=10, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m³a Vol]") +
	ggtitle("CO2 per sector normalized by building volume.")
ggsave(file="figures/boxplot_industrial_norm_vol.png")

stats = boxplot.stats(industrial$ratio_ground)$stats
iqr = IQR(industrial$ratio_ground)
skew_gpc = round(skewness(industrial_gpc$ratio_ground), 2)
skew_ccr = round(skewness(industrial_ccr$ratio_ground), 2)
skew_eco = round(skewness(industrial_eco$ratio_ground), 2)
ggplot(industrial, aes(x=sector_name, y=ratio_ground)) +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", shape=8, size=4) +
	geom_text(data=NULL, x=1, y=50, label=skew_ccr) +
	geom_text(data=NULL, x=2, y=50, label=skew_gpc) +
	geom_text(data=NULL, x=3, y=50, label=skew_eco) +
	coord_cartesian(ylim = c(0, stats[5] + 3*iqr)) + 
	xlab("Sectors") +
	ylab("CO2 [kg/m²a GroundArea]") +
	ggtitle("CO2 per sector normalized by building's ground surface.")
ggsave(file="figures/boxplot_industrial_norm_ground.png")

odbcClose(conn)
