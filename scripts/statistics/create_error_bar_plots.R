# Counts number of buildings associated to each sector. 
# Distributes buildings with 1:n mappings based on normal distribution.

rm(list=ls())

library(RODBCext)
library(ggplot2)
library(dplyr)

add_to_buildings <- function(sector_id, add_buildings, add) {
	current_val <- add_buildings[add_buildings$sector_id==sector_id, 2]
	add_buildings[add_buildings$sector_id==sector_id, 2] = current_val + add
	return(add_buildings)
}

# Distribute buildings for given sector.
# row: row of frequence table
distribute_buildings <- function(row, nSectors, add_buildings) {
	freq <- row[nSectors+1]
	freq <- strtoi(freq)
	if (freq %% nSectors == 0) {
		add <- freq/nSectors
		for (i in 1:nSectors) {
			s <- row[i]
			add_buildings <- add_to_buildings(s, add_buildings, add)
		}
	}
	else if (freq %% nSectors == 1){
		add <- (freq-1)/nSectors
		for (i in 1:nSectors) {
			s <- row[i]
			add_buildings <- add_to_buildings(s, add_buildings, add)
		}
		s <- row[1]
		add_buildings <- add_to_buildings(s, add_buildings, 1)
	} else if (freq %% nSectors == 2) {
		add <- (freq-2)/nSectors
		for (i in 1:nSectors) {
			s <- row[i]
			add_buildings <- add_to_buildings(s, add_buildings, add)
		}
		s <- row[1]
		add_buildings <- add_to_buildings(s, add_buildings, 1)
		s <- row[2]
		add_buildings <- add_to_buildings(s, add_buildings, 1)
	}
	additional_buildings=add_buildings
}

create_mapping_error_bar_plots <- function(protocol) {

	conn <- odbcConnect("local", uid="richard", case="postgresql")
	query <- readLines("select_sector_mappings.sql")
	query <- paste(query, collapse=" \n ")
	query=gsub("XXXX", protocol, query)
	mappings <- sqlExecute(conn, query, NULL, fetch=T)
	odbcClose(conn)

	mappings[is.na(mappings$sector_id), ] <- 'NA'
	mappings[is.na(mappings$sector_name), ] <- 'NA'
	sector_ids <- unique(mappings$sector_id)

	sector_names <- unique(select(mappings, sector_id, sector_name))
	sector_names$sector_id <- factor(sector_names$sector_id)
	sector_names$sector_name=gsub(" ", "\n", sector_names$sector_name)
	s_all <- c()
	s_only <- c()
	for (i in sector_ids) {
		sec <- filter(mappings, sector_id==i)
		sec_only <- filter(sec, is.na(sector_id_2))
		s_mixed <- anti_join(sec, sec_only, by="id")

		# number of buildings with 1:1 mapping in current sector
		section_only <- nrow(sec_only)
		s_only <- c(s_only, section_only)

		# number of buildings with 1:n mapping in current sector
		section_all <- bind_rows(sec, filter(mappings, sector_id_2==i)) 
		section_all <- bind_rows(section_all, filter(mappings, sector_id_3==i))
		s <- nrow(section_all)
		s_all <- c(s_all, s)
	}
	sector_counts <- data.frame(sector_ids, s_all)
	sector_counts <- cbind(sector_counts, s_only)

	# distribute buildings with 1:n mapping to sectors
	three_sectors <- filter(mappings, !is.na(sector_id_2), !is.na(sector_id_3))
	three_combinations <- unique(three_sectors[, 2:4])
	frequences <- plyr::count(three_sectors, vars=c("sector_id", "sector_id_2", "sector_id_3"))

	tmp <- vector(mode="numeric", length=length(sector_ids))
	additional_buildings <- data.frame(sector_ids, tmp)
	add <- apply(frequences[, c("sector_id", "sector_id_2", "sector_id_3", "freq")], 1, distribute_buildings, nSectors=3, add_buildings=additional_buildings)

	two_sectors <- filter(mappings, !is.na(sector_id_2), is.na(sector_id_3))
	two_sectors <- select(two_sectors, `id`, sector_id, sector_id_2)
	frequences <- plyr::count(two_sectors, vars=c("sector_id", "sector_id_2"))
	add2 <- apply(frequences[, c("sector_id", "sector_id_2", "freq")], 1, distribute_buildings, nSectors=2, add_buildings=additional_buildings)
	add <- c(add, add2)
	# calculate absolute number of distributed buildings
	s_dist <- sector_counts$s_only
	for (i in 1:length(add)) {
		a <- add[[i]]
		for (j in 1:length(s_dist)) {
			s_dist[j] <- s_dist[j] + a[sector_ids==sector_counts$sector_ids[j], 2]
		}
	}
	sector_counts <- cbind(sector_counts, s_dist)

	sector_counts$s_all <- sector_counts$s_all - sector_counts$s_dist
	sector_counts$s_only <- sector_counts$s_dist - sector_counts$s_only
	sector_counts <- inner_join(sector_counts, sector_names, by=c("sector_ids" = "sector_id"))

	if (nrow(sector_counts[sector_counts$sector_ids=='NA',]) != 0) {
		sector_counts[sector_counts$sector_ids=='NA',]$s_all = 0
		sector_counts[sector_counts$sector_ids=='NA',]$s_only = 0
	}

	df <- data.frame(
			 sector_name = sector_counts$sector_name,
			 number = sector_counts$s_dist,
			 min = sector_counts$s_only,
			 max = sector_counts$s_all
			 )
	filename=paste0("error_bars_", protocol, ".png")
	limits <- aes(ymax = number + max, ymin = number - min)
	dodge <- position_dodge(width=0.9)
	ggplot(df, aes(y=number, x=sector_name)) +
	geom_bar(stat="identity", fill="#FF9999") +
	geom_errorbar(limits, position=dodge, width=0.5)
	ggsave(file=filename)
}

create_mapping_error_bar_plots("gpc")
create_mappig_error_bar_plots("ecoregion")
