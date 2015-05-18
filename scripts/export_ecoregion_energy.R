# Exports energy consumption per sector per energy carriers from db.
# Calculates energy demands per energy source as sum of
#	- energy demand for buildings with one energy source, and
#	- energy demand for buildings with combined energy sources. The partitioning of a building's energy demand per source is done by distributing the energy demand to each energy source with the same probability. E.g. if a building's energy demand is 900 MWh/a ans the building's energy sources are gas, strom and coal, then each source is assigned with 300MWh/a. If we have more accurate data about the share, this calculation can be done more accurate.

# TODO: Add all sectors.

rm(list=ls())

library(RODBCext)
library(dplyr)

get_cols <- function(data, ...) {
  data %>% select(...)
}

get_energy_source <- function(data, ...) {
	data %>%
	get_cols(...) %>%
	bind_cols(gid) %>%
	bind_cols(value)
}

conn <- odbcConnect("local", uid="richard", case="postgresql")

query <- readLines("ecoregion_energy.sql")
query <- paste(query, collapse=" \n ")
eco_energy <- sqlExecute(conn, query, NULL, fetch=T) 
odbcClose(conn)

gid <- select(eco_energy, matches("gid"))
value <- select(eco_energy, matches("i_qe"))

# Extract energy values per energy source
gas <- eco_energy %>%
	get_energy_source(d_gas_ewp, d_gas_ep, d_gas_lt) %>%
	rowwise() %>%
	mutate(gas=max(d_gas_ewp, d_gas_ep, d_gas_lt)) %>%
	filter(gas==1) %>% 
	select(-d_gas_ewp) %>%
	select(-d_gas_ep) %>%
	select(-d_gas_lt)

oil <- eco_energy %>%
	get_energy_source(d_oel_kat, d_oel_ep, d_oel_lt) %>%
	rowwise() %>%
	mutate(oil=max(d_oel_kat, d_oel_ep, d_oel_lt)) %>%
	filter(oil==1) %>%
	select(-d_oel_kat) %>%
	select(-d_oel_ep) %>%
	select(-d_oel_lt)

strom <- eco_energy %>%
	get_energy_source(d_strom_lt) %>%
	rowwise() %>%
	mutate(strom=max(d_strom_lt)) %>%
	filter(strom==1) %>%
	select(-d_strom_lt)

fw <- eco_energy %>%
	get_energy_source(d_fw_ep, d_fw_ewp, d_fw_lt) %>%
	rowwise() %>%
	mutate(fw=max(d_fw_ep, d_fw_ewp, d_fw_lt)) %>%
	filter(fw==1) %>%
	select(-d_fw_ewp) %>%
	select(-d_fw_ep) %>%
       	select(-d_fw_lt)

coal <- eco_energy %>%
	get_energy_source(d_kohle_ep, d_kohle_lt) %>%
	rowwise() %>%
	mutate(coal=max(d_kohle_ep, d_kohle_lt)) %>%
	filter(coal==1) %>%
	select(-d_kohle_ep) %>%
       	select(-d_kohle_lt)

wp <- eco_energy %>%
	get_energy_source(d_wp_kat) %>%
	rowwise() %>%
	mutate(wp=max(d_wp_kat)) %>%
	filter(wp==1) %>%
	select(-d_wp_kat)

# Extract rows (buildings) with only one energy source
gas_only <- gas %>%
	anti_join(oil, by="gid") %>%
	anti_join(strom, by="gid") %>%
	anti_join(fw, by="gid") %>%
	anti_join(coal, by="gid") %>%
	anti_join(wp, by="gid")

oil_only <- oil %>%
	anti_join(gas, by="gid") %>%
	anti_join(strom, by="gid") %>%
	anti_join(fw, by="gid") %>%
	anti_join(coal, by="gid") %>%
	anti_join(wp, by="gid")

strom_only <- strom %>%
	anti_join(gas, by="gid") %>%
	anti_join(oil, by="gid") %>%
	anti_join(fw, by="gid") %>%
	anti_join(coal, by="gid") %>%
	anti_join(wp, by="gid")

fw_only <- fw %>%
	anti_join(gas, by="gid") %>%
	anti_join(strom, by="gid") %>%
	anti_join(oil, by="gid") %>%
	anti_join(coal, by="gid") %>%
	anti_join(wp, by="gid")

coal_only <- coal %>%
	anti_join(gas, by="gid") %>%
	anti_join(strom, by="gid") %>%
	anti_join(fw, by="gid") %>%
	anti_join(oil, by="gid") %>%
	anti_join(wp, by="gid")

wp_only <- wp %>%
	anti_join(gas, by="gid") %>%
	anti_join(strom, by="gid") %>%
	anti_join(fw, by="gid") %>%
	anti_join(coal, by="gid") %>%
	anti_join(oil, by="gid")

# Calculate energy demand per energy source for buildings with one energy source
energy_gas <- sum(gas_only$i_qe)
energy_oil <- sum(oil_only$i_qe)
energy_strom <- sum(strom_only$i_qe)
energy_fw <- sum(fw_only$i_qe)
energy_coal <- sum(coal_only$i_qe)
energy_wp <- sum(wp_only$i_qe)

# Get buildings (rows) with more than one energy source
g_mixed <- anti_join(gas, gas_only, by="gid")
o_mixed <- anti_join(oil, oil_only, by="gid")
s_mixed <- anti_join(strom, strom_only, by="gid")
f_mixed <- anti_join(fw, fw_only, by="gid")
c_mixed <- anti_join(coal, coal_only, by="gid")
w_mixed <- anti_join(wp, wp_only, by="gid")

# Combine energy sources.
gas_mixed <- g_mixed %>%
	left_join(o_mixed, by="gid") %>%
	select(-i_qe.y) %>%
	left_join(s_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(f_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(c_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(w_mixed, by="gid") %>%
	select(-i_qe) %>%
	# Calculate number of energy sources per buildings
	rowwise() %>% 
	mutate(n=sum(gas, oil, strom, fw, coal, wp, na.rm=T)) %>%
	# Scale energy demand by number of energy sources
	mutate(val=i_qe.x/n)

oil_mixed <- o_mixed %>%
	left_join(g_mixed, by="gid") %>%
	select(-i_qe.y) %>%
	left_join(s_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(f_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(c_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(w_mixed, by="gid") %>%
	select(-i_qe) %>%
	# Calculate number of energy sources per buildings
	rowwise() %>% 
	mutate(n=sum(gas, oil, strom, fw, coal, wp, na.rm=T)) %>%
	mutate(val=i_qe.x/n)

strom_mixed <- s_mixed %>%
	left_join(o_mixed, by="gid") %>%
	select(-i_qe.y) %>%
	left_join(g_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(f_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(c_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(w_mixed, by="gid") %>%
	select(-i_qe) %>%
	# Calculate number of energy sources per buildings
	rowwise() %>% 
	mutate(n=sum(gas, oil, strom, fw, coal, wp, na.rm=T)) %>%
	mutate(val=i_qe.x/n)

fw_mixed <- f_mixed %>%
	left_join(o_mixed, by="gid") %>%
	select(-i_qe.y) %>%
	left_join(s_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(g_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(c_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(w_mixed, by="gid") %>%
	select(-i_qe) %>%
	# Calculate number of energy sources per buildings
	rowwise() %>% 
	mutate(n=sum(gas, oil, strom, fw, coal, wp, na.rm=T)) %>%
	mutate(val=i_qe.x/n)

coal_mixed <- c_mixed %>%
	left_join(o_mixed, by="gid") %>%
	select(-i_qe.y) %>%
	left_join(s_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(f_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(g_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(w_mixed, by="gid") %>%
	select(-i_qe) %>%
	# Calculate number of energy sources per buildings
	rowwise() %>% 
	mutate(n=sum(gas, oil, strom, fw, coal, wp, na.rm=T)) %>%
	mutate(val=i_qe.x/n)

wp_mixed <- w_mixed %>%
	left_join(o_mixed, by="gid") %>%
	select(-i_qe.y) %>%
	left_join(s_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(f_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(c_mixed, by="gid") %>%
	select(-i_qe) %>%
	left_join(g_mixed, by="gid") %>%
	select(-i_qe) %>%
	# Calculate number of energy sources per buildings
	rowwise() %>% 
	mutate(n=sum(gas, oil, strom, fw, coal, wp, na.rm=T)) %>%
	mutate(val=i_qe.x/n)

energy_gas <- energy_gas + sum(gas_mixed$val)
energy_oil <- energy_oil + sum(oil_mixed$val)
energy_strom <- energy_strom + sum(strom_mixed$val)
energy_fw <- energy_fw + sum(fw_mixed$val)
energy_coal <- energy_coal + sum(coal_mixed$val)
energy_wp <- energy_wp + sum(wp_mixed$val)
energy_sum <- energy_gas + energy_oil + energy_strom + energy_fw + energy_coal

energy_carrier <- c("Power", "EL heating oil", "Natural gas", "District heat", "Wood", "Environmental heat", "Solar collectors", "Biogases", "Waste", "Liquid gas", "Vegetable oil", "Lignite", "Coal", "Total");
x2011 <- c(energy_strom, energy_oil, energy_gas, energy_fw, 0, 0, 0, 0, 0, 0, 0, energy_coal, 0, energy_sum)
result = data.frame(energy_carrier, x2011)
write.csv(result, "ecoregion.csv", row.names=FALSE)

