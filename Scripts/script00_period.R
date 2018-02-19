library(dplyr)
library(lubridate)

source("./Scripts/template.R")

# file.path(".", "Data", "ref_ratings.RData") %>% load()
load("~/GitHub/ratings-migration/Data/ref_ratings.RData")

ratings.df <- file.path(".", "Data", "data00_sanitised rating actions.rds") %>% readRDS()

# Create period template ####

max.date <- as.Date("2012-08-24") # Last update
end.period <- eoMonth(year(max.date), month(max.date), isWeekday = FALSE)

## Loop

countries.ls <- as.list(unique(ratings.df$Country))

periods.ls <- lapply(
	countries.ls,
	function(temp.country){
		# temp.country <- "Philippines"
		
		test.df <- ratings.df %>% 
			filter(Country == temp.country) %>% 
			arrange(Date)
		
		### Start period
		
		test.min.date <- min(test.df$Date)
		
		start.period <- eoMonth(year(test.min.date), month(test.min.date), isWeekday = FALSE)
		
		periods <- seq.Date(start.period + 1, end.period + 1, by = "month") - 1
			## plus one- minus one used as work around to buggy non-31-day months
		
		entry.idx <- findInterval(periods, test.df$Date)
		
		periods.df <- test.df[entry.idx,] %>% 
			rename(Date.updated = Date) %>% 
			mutate(Month.end = periods) %>% 
			filter(FC.rating.outlook != "Withdrawn") ## Assumes that all "withdrawn" entries are proper
		
		return(periods.df)
	}
)

periods.df <- do.call(bind_rows, periods.ls)

periods.df <- periods.df %>% 
	select(
		Country, Month.end,
		FC.rating.long, FC.rating.short, FC.rating.outlook,
		LC.rating.long, LC.rating.outlook,
		Date.updated
	) %>% 
	mutate(
		FC.rating.long = replace(FC.rating.long, FC.rating.long == "-", NA) %>% 
			factor(levels = longterm.rating.levels)
	)

period.name <- "data01_period rating view"
csv.file <- paste0(period.name, ".csv")
rds.file <- paste0(period.name, ".rds")

saveRDS(periods.df, file.path(".", "Data", rds.file))
write.csv(periods.df, file.path(".", "Data", csv.file), row.names = FALSE)

# Sankey ####

library(ggalluvial)
library(writexl)

period.df <- periods.df %>% 
	filter(!is.na(FC.rating.long)) %>% 
	filter(year(Month.end) >= 2006, month(Month.end) == 12) %>% # year-end since 2006
	mutate(
		Month = month(Month.end, label = TRUE, abbr = TRUE) %>% factor(levels = month.abb),
		Year = year(Month.end) %>% paste("CY", .)
	)

period.df %>% 
	select(Country, Month.end, Year, FC.rating.long) %>% 
	mutate(
		Rating.level = length(longterm.rating.levels) - as.integer(FC.rating.long) + 1 - 1
	) %>% 
	write_xlsx("./Data/data02_yearend-since2006.xlsx")

longterm.rating.cols <- c(
	"deepskyblue", # AAA
	"cyan3", "cyan2", "cyan1", # AA
	"springgreen3", "springgreen2", "springgreen1", # AA
	"chartreuse3", "chartreuse2", "chartreuse1", # BBB
	# Investment grade
	"yellow3", "yellow2", "yellow1", # BB
	"goldenrod3", "goldenrod2", "goldenrod1", # B
	"orange3", "orange2", "orange1", # CCC
	"orangered1", # CC
	"orangered2", # C
	# Default
	"red1", # RD
	"#787c84" # D
)
names(longterm.rating.cols) <- longterm.rating.levels

ggplot(period.df) +
	aes(
		x = Year, stratum = FC.rating.long, alluvium = Country,
		fill = FC.rating.long
	) +
	geom_stratum() +
	geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = NA) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_discrete(name = NULL) +
	scale_fill_manual(name = "Foreign CCY\nlong-term", values = longterm.rating.cols, na.value = NA) +
	theme(
		plot.background = element_blank(),
		panel.background = element_blank(),
		axis.ticks = element_blank(),
		axis.text.y = element_blank()
	)
ggsave("./Plots/plot00_sankey_yearend-since2006.png")	

ggplot(period.df) +
	aes(
		x = Year, stratum = FC.rating.long, alluvium = Country,
		fill = FC.rating.long
	) +
	geom_stratum() +
	# geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = NA) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_discrete(name = NULL) +
	scale_fill_manual(name = "Foreign CCY\nlong-term", values = longterm.rating.cols, na.value = NA) +
	theme(
		plot.background = element_blank(),
		panel.background = element_blank(),
		axis.ticks = element_blank(),
		axis.text.y = element_blank()
	)
ggsave("./Plots/plot01_bar_yearend-since2006.png")	

library(writexl)

period.df %>% 
	group_by(Year, FC.rating.long) %>% 
	summarise(
		Countries = length(unique(Country))
	) %>% 
	write_xlsx("./Data/data03_yearend-counts.xlsx")
