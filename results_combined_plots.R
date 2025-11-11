## Objective of this script: create plot aggregations, tables, maps to be embedded in the thesis

install.packages("modelsummary")
install.packages("gt")
install.packages("webshot2")
install.packages("patchwork") 
library(patchwork)
library(grid)
library(ggplot2)
library(scales)
library(readr)
library(tidyr)
library(tidyverse)
library(modelsummary)
library(gt)
library(webshot2)
library(dplyr)

## make sure previously created plots are loaded into the environment


#### HEAT RISK INDEX ####
combined_plot_risk <- wrap_plots(
  plot_vulnerability_index_athens + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vulnerability_index_bologna + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vulnerability_index_barcelona + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vulnerability_index_vienna + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Heat Risk Index",
    subtitle = "Index Values Only Comparable Intra-City",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_risk)

## Save plot
ggsave(
  filename = "Data/combinedplots/heatrisk_plot.png",
  plot     = combined_plot_risk,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### HEAT RISK INDEX DISTRIBUTION ####
combined_plot_risk_distr <- wrap_plots(
  plot_athens_vulnerability_distribution + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_bologna_vulnerability_distribution + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_barcelona_vulnerability_distribution + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vienna_vulnerability_distribution + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Distribution of Heat Risk Index Values",
    subtitle = "Index Values Only Comparable Intra-City",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_risk_distr)

## Save plot
ggsave(
  filename = "Data/combinedplots/heatrisk_distribution.png",
  plot     = combined_plot_risk_distr,
  width    = 8,
  height   = 6,
  dpi      = 300
)



#### HEAT RISK INDEX & CC ####
combined_plot_risk_cc <- wrap_plots(
  plot_vulnerability_index_athens_cc + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vulnerability_index_bologna_cc + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vulnerability_index_barcelona_cc + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vulnerability_index_vienna_cc + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Existing Cooling Centres Against Heat Risk Index",
    subtitle = "Index Values Only Comparable Intra-City",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

print(combined_plot_risk_cc)

## Save plot
ggsave(
  filename = "Data/combinedplots/heatrisk_cc_plot.png",
  plot     = combined_plot_risk_cc,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### POPULATION DENSITY ####
combined_plot_pop <- wrap_plots(
  plot_pop_athens + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_pop_bologna + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_pop_barcelona + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_pop_vienna + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Population Density Index",
    subtitle = "Index Values Only Comparable Intra-City",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_pop)

## Save plot
ggsave(
  filename = "Data/combinedplots/pop_density_plot.png",
  plot     = combined_plot_pop,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### HEAT STRESS ####
combined_plot_heatstress <- wrap_plots(
  plot_heat_index_athens + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_heat_index_bologna + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_heat_index_barcelona + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_heat_index_vienna + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Heat Stress Index",
    subtitle = "Index Values Only Comparable Intra-City",
    caption = "White Spaces: Missing Data Explained in Description Below",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_heatstress)

## Save plot
ggsave(
  filename = "Data/combinedplots/heat_stress.png",
  plot     = combined_plot_heatstress,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### BUILDING DENSITY ####
combined_plot_builddens <- wrap_plots(
  plot_builddens_athens_index + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_builddens_bologna_index + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_builddens_barcelona_index + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_builddens_vienna_index + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Building Density Index",
    subtitle = "Index Values Only Comparable Intra-City",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_builddens)

## Save plot
ggsave(
  filename = "Data/combinedplots/buildind_density.png",
  plot     = combined_plot_builddens,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### VEGETATION COVER ####
combined_plot_veg <- wrap_plots(
  plot_veg_index_athens + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_veg_index_bologna + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_veg_index_barcelona + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_veg_index_vienna + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Green Vegetation Cover Index",
    subtitle = "Index Values Only Comparable Intra-City",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_veg)

## Save plot
ggsave(
  filename = "Data/combinedplots/veg_cover.png",
  plot     = combined_plot_veg,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### AGE ####
combined_plot_age <- wrap_plots(
  plot_age_athens + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_age_bologna + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_age_barcelona + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_age_vienna + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Elderly Population Index",
    subtitle = "Index Values Only Comparable Intra-City",
    caption = "White Spaces: No Population Aged >65 Years",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_age)

## Save plot
ggsave(
  filename = "Data/combinedplots/age.png",
  plot     = combined_plot_age,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### INCOME ####
combined_plot_income <- wrap_plots(
  plot_income_athens + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_income_bologna + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_income_barcelona + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_income_vienna + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  guides = "collect",
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Income Vulnerability Index",
    subtitle = "Index Values Only Comparable Intra-City",
    caption = "1 = High Vulnerability Due To Low Income;
    Different Granularity Based On Available Data",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_income)

## Save plot
ggsave(
  filename = "Data/combinedplots/income.png",
  plot     = combined_plot_income,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### TRAVEL TIME #### NOT USED BC OF DIFFERENT SCALES
combined_plot_tt <- wrap_plots(
  athens_cc_traveltime_plot + 
    ggtitle("Athens", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  bologna_cc_traveltime_plot + 
    ggtitle("Bologna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  barcelona_cc_traveltime_plot + 
    ggtitle("Barcelona", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  vienna_cc_traveltime_plot + 
    ggtitle("Vienna", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Travel Times to Existing Cooling Centres",
    subtitle = "Index Values Only Comparable Intra-City",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_tt)

## Save plot
ggsave(
  filename = "Data/combinedplots/traveltime_existing_cc.png",
  plot     = combined_plot_tt,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### ACCESSIBILITY ATHENS ####
combined_plot_tt_athens <- wrap_plots(
  plot_vulnerability_index_athens_cc + 
    ggtitle("Locations Against Heat Risk Index", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  athens_cc_traveltime_plot + 
    ggtitle("Walking Travel Times", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_traveltime_stats_athens +
    ggtitle("Population Within Walking Time Thresholds", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 3,
  nrow = 1,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Athens: Accessibility of Existing Cooling Centres",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_tt_athens)

## Save plot
ggsave(
  filename = "Data/combinedplots/accessibility_athens.png",
  plot     = combined_plot_tt_athens,
  width    = 12,
  height   = 4,
  dpi      = 300
)

#### ACCESSIBILITY BARCELONA ####
combined_plot_tt_barcelona <- wrap_plots(
  plot_vulnerability_index_barcelona_cc + 
    ggtitle("Locations Against Heat Risk Index", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  barcelona_cc_traveltime_plot + 
    ggtitle("Walking Time Coverage", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_traveltime_stats_barcelona +
    ggtitle("Population Within Walking Time Thresholds", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 3,
  nrow = 1,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Barcelona: Accessibility of Existing Cooling Centres",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )


print(combined_plot_tt_barcelona)

## Save plot
ggsave(
  filename = "Data/combinedplots/accessibility_barcelona.png",
  plot     = combined_plot_tt_barcelona,
  width    = 12,
  height   = 4,
  dpi      = 300
)



#### ACCESSIBILITY BOLOGNA ####
combined_plot_tt_bologna <- wrap_plots(
  plot_vulnerability_index_bologna_cc + 
    ggtitle("Locations Against Heat Risk Index", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  bologna_cc_traveltime_plot + 
    ggtitle("Walking Travel Times", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_traveltime_stats_bologna +
    ggtitle("Population Within Walking Time Thresholds", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 3,
  nrow = 1,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Bologna: Accessibility of Existing Cooling Centres",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_tt_bologna)

## Save plot
ggsave(
  filename = "Data/combinedplots/accessibility_bologna.png",
  plot     = combined_plot_tt_bologna,
  width    = 12,
  height   = 4,
  dpi      = 300
)


#### ACCESSIBILITY VIENNA ####
combined_plot_tt_vienna <- wrap_plots(
  plot_vulnerability_index_vienna_cc + 
    ggtitle("Locations Against Heat Risk Index", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  vienna_cc_traveltime_plot + 
    ggtitle("Walking Travel Times", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_traveltime_stats_vienna +
    ggtitle("Population Within Walking Time Thresholds", subtitle = " ") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 3,
  nrow = 1,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Vienna: Accessibility of Existing Cooling Centres",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_tt_vienna)

## Save plot
ggsave(
  filename = "Data/combinedplots/accessibility_vienna.png",
  plot     = combined_plot_tt_vienna,
  width    = 12,
  height   = 4,
  dpi      = 300
)


#### ALLOCATION ATHENS ####
combined_plot_allocation_athens <- wrap_plots(
  athens_cc_traveltime_plot + 
    ggtitle("Current Situation", subtitle = "68% Coverage (15 Min)\nAchieved with 7 CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_athens_s1 + 
    ggtitle("Scenario 1", subtitle = "Objective: 80% Coverage (15 Min)\nAchieved with 2 add. CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_athens_s2 + 
    ggtitle("Scenario 2", subtitle = "Objective: 99% Coverage (15 Min)\nAchieved with 17 add. CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_athens_s3 +
    ggtitle("Scenario 3", subtitle = "Objective: Allocate 3 Most Needed Facilities\nAchieves 92% Coverage (15 Min)") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Athens: Allocating Additional Cooling Centres in Three Scenarios",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_allocation_athens)

## Save plot
ggsave(
  filename = "Data/combinedplots/allocation_athens.png",
  plot     = combined_plot_allocation_athens,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### ALLOCATION BARCELONA ####
combined_plot_allocation_barcelona <- wrap_plots(
  barcelona_cc_traveltime_plot + 
    ggtitle("Current Situation", subtitle = "94% Coverage (15 Min)\nAchieved with 322 CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_barcelona_s3 +
    ggtitle("Scenario 3", subtitle = "Objective: Allocate 3 Most Needed Facilities\nAchieves 99% Coverage (15 Min)") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 1,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Barcelona: Allocating Additional Cooling Centres in Scenario 3",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_allocation_barcelona)

## Save plot
ggsave(
  filename = "Data/combinedplots/allocation_barcelona.png",
  plot     = combined_plot_allocation_barcelona,
  width    = 8,
  height   = 3,
  dpi      = 300
)



#### ALLOCATION BOLOGNA ####
combined_plot_allocation_bologna <- wrap_plots(
  bologna_cc_traveltime_plot + 
    ggtitle("Current Situation", subtitle = "37% Coverage (15 Min)\nAchieved with 7 CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_bologna_s1 + 
    ggtitle("Scenario 1", subtitle = "Objective: 80% Coverage (15 Min)\nAchieved with 5 add. CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_bologna_s2 + 
    ggtitle("Scenario 2", subtitle = "Objective: 99% Coverage (15 Min)\nAchieved with 97 add. CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_bologna_s3 +
    ggtitle("Scenario 3", subtitle = "Objective: Allocate 3 Most Needed Facilities\nAchieves 77% Coverage (15 Min)") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Bologna: Allocating Additional Cooling Centres in Three Scenarios",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_allocation_bologna)

## Save plot
ggsave(
  filename = "Data/combinedplots/allocation_bologna.png",
  plot     = combined_plot_allocation_bologna,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### ALLOCATION VIENNA ####
combined_plot_allocation_vienna <- wrap_plots(
  vienna_cc_traveltime_plot + 
    ggtitle("Current Situation", subtitle = "42% Coverage (15 Min)\nAchieved with 25 CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vienna_s1 + 
    ggtitle("Scenario 1", subtitle = "Objective: 80% Coverage (15 Min)\nAchieved with 10 add. CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vienna_s2 + 
    ggtitle("Scenario 2", subtitle = "Objective: 99% Coverage (15 Min) - failed\nAchieved 97% with 283 add. CC") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  plot_vienna_s3 +
    ggtitle("Scenario 3", subtitle = "Objective: Allocate 3 Most Needed Facilities\nAchieves 82% Coverage (15 Min)") +
    theme(plot.title = element_text(size = 12, face = "bold")),
  
  ncol = 2,
  nrow = 2,
  heights = unit(c(1, 1), "null"),  # equal vertical spacing
  widths = unit(c(1, 1), "null")    # equal horizontal spacing
) +
  plot_annotation(
    title = "Vienna: Allocating Additional Cooling Centres in Three Scenarios",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(combined_plot_allocation_vienna)

## Save plot
ggsave(
  filename = "Data/combinedplots/allocation_vienna.png",
  plot     = combined_plot_allocation_vienna,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#### FACILITY TYPES DISTRIBUTION ATHENS ####
## read csv
types_distribution_athens <- read.csv("Data/allocation/types_distribution_athens.csv", sep = ";")

## Rename columns and reshape to long
df_types_long_athens <- types_distribution_athens %>%
  rename(
    `Current Situation` = Current.Situation,
    `Scenario 1` = Scenario.1,
    `Scenario 2` = Scenario.2,
    `Scenario 3` = Scenario.3
  ) %>%
  pivot_longer(
    cols = c(`Current Situation`, `Scenario 1`, `Scenario 2`, `Scenario 3`),
    names_to = "Scenario",
    values_to = "Value"
  )

## Grouped bar chart with ggplot2
plot_types_athens <- ggplot(df_types_long_athens,
       aes(x = Facility.Types, y = Value, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "Facility Types",
    y = "% of Type in Network",
    fill = "Scenario"
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  plot_annotation(
    title = "Athens: Relative Share of Facility Types within the Network per Scenario",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )


print(plot_types_athens)

## Save plot
ggsave(
  filename = "Data/combinedplots/facility_types_athens.png",
  plot     = plot_types_athens,
  width    = 12,
  height   = 4,
  dpi      = 300
)

#### FACILITY TYPES DISTRIBUTION BARCELONA ####
## read csv
types_distribution_barcelona <- read.csv("Data/allocation/types_distribution_barcelona.csv", sep = ";")

## Rename columns and reshape to long
df_types_long_barcelona <- types_distribution_barcelona %>%
  rename(
    `Current Situation` = Current.Situation,
    `Scenario 1` = Scenario.1,
    `Scenario 2` = Scenario.2,
    `Scenario 3` = Scenario.3
  ) %>%
  pivot_longer(
    cols = c(`Current Situation`, `Scenario 1`, `Scenario 2`, `Scenario 3`),
    names_to = "Scenario",
    values_to = "Value"
  )

## Grouped bar chart with ggplot2
plot_types_barcelona <- ggplot(df_types_long_barcelona,
                            aes(x = Facility.Types, y = Value, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "Facility Types",
    y = "% of Type in Network",
    fill = "Scenario"
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  plot_annotation(
    title = "Barcelona: Relative Share of Facility Types within the Network per Scenario",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )


print(plot_types_barcelona)

## Save plot
ggsave(
  filename = "Data/combinedplots/facility_types_barcelona.png",
  plot     = plot_types_barcelona,
  width    = 12,
  height   = 4,
  dpi      = 300
)


#### FACILITY TYPES DISTRIBUTION BOLOGNA ####
## read csv
types_distribution_bologna <- read.csv("Data/allocation/types_distribution_bologna.csv", sep = ";")

## Rename columns and reshape to long
df_types_long_bologna <- types_distribution_bologna %>%
  rename(
    `Current Situation` = Current.Situation,
    `Scenario 1` = Scenario.1,
    `Scenario 2` = Scenario.2,
    `Scenario 3` = Scenario.3
  ) %>%
  pivot_longer(
    cols = c(`Current Situation`, `Scenario 1`, `Scenario 2`, `Scenario 3`),
    names_to = "Scenario",
    values_to = "Value"
  )

## Grouped bar chart with ggplot2
plot_types_bologna <- ggplot(df_types_long_bologna,
                               aes(x = Facility.Types, y = Value, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "Facility Types",
    y = "% of Type in Network",
    fill = "Scenario"
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  plot_annotation(
    title = "Bologna: Relative Share of Facility Types within the Network per Scenario",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )


print(plot_types_bologna)

## Save plot
ggsave(
  filename = "Data/combinedplots/facility_types_bologna.png",
  plot     = plot_types_bologna,
  width    = 12,
  height   = 4,
  dpi      = 300
)


#### FACILITY TYPES DISTRIBUTION VIENNA ####
## read csv
types_distribution_vienna <- read.csv("Data/allocation/types_distribution_vienna.csv", sep = ";")

## Rename columns and reshape to long
df_types_long_vienna <- types_distribution_vienna %>%
  rename(
    `Current Situation` = Current.Situation,
    `Scenario 1` = Scenario.1,
    `Scenario 2` = Scenario.2,
    `Scenario 3` = Scenario.3
  ) %>%
  pivot_longer(
    cols = c(`Current Situation`, `Scenario 1`, `Scenario 2`, `Scenario 3`),
    names_to = "Scenario",
    values_to = "Value"
  )

## Grouped bar chart with ggplot2
plot_types_vienna <- ggplot(df_types_long_vienna,
                               aes(x = Facility.Types, y = Value, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "Facility Types",
    y = "% of Type in Network",
    fill = "Scenario"
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  plot_annotation(
    title = "Vienna: Relative Share of Facility Types within the Network per Scenario",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )


print(plot_types_vienna)

## Save plot
ggsave(
  filename = "Data/combinedplots/facility_types_vienna.png",
  plot     = plot_types_vienna,
  width    = 12,
  height   = 4,
  dpi      = 300
)


#### INDEX INPUT DATA VALUE TABLE #####
# Combining Data Frames with Index Values
combined_input_data <- bind_rows(
  #Athens
  pop_athens_df %>% 
    rename(index_value = pop.index) %>%
    mutate(City = "Athens", `Input Factor` = "Population Density"),
  heat_index_athens_df %>% 
    rename(index_value = heat_index_athens) %>%
    mutate(City = "Athens", `Input Factor` = "Heat Stress"),
  builddens_athens_index_df %>% 
    rename(index_value = builddens_index_athens) %>%
    mutate(City = "Athens", `Input Factor` = "Building Density"),
  veg_vulnerability_index_athens_df %>%
    rename(index_value = veg_index_vuln_athens) %>%
    mutate(City = "Athens", `Input Factor` = "Vegetation Cover Vulnerability"),
  age_athens_index %>% 
    dplyr::select(-geom) %>%
    rename(index_value = age.index) %>%
    mutate(City = "Athens", `Input Factor` = "Elderly Population"),
  income_athens_index_df %>% 
    rename(index_value = income.index) %>%
    mutate(City = "Athens", `Input Factor` = "Income Vulnerability"),
  # Barcelona
  pop_barcelona_df %>% 
    rename(index_value = pop.index) %>%
    mutate(City = "Barcelona", `Input Factor` = "Population Density"),
  heat_index_barcelona_df %>% 
    rename(index_value = heat_index_barcelona) %>%
    mutate(City = "Barcelona", `Input Factor` = "Heat Stress"),
  builddens_barcelona_index_df %>% 
    rename(index_value = builddens_index_barcelona) %>%
    mutate(City = "Barcelona", `Input Factor` = "Building Density"),
  veg_vulnerability_index_barcelona_df %>%
    rename(index_value = veg_index_vuln_barcelona) %>%
    mutate(City = "Barcelona", `Input Factor` = "Vegetation Cover Vulnerability"),
  age_barcelona_index %>% 
    dplyr::select(-geom) %>%
    rename(index_value = age.index) %>%
    mutate(City = "Barcelona", `Input Factor` = "Elderly Population"),
  income_barcelona_index %>% 
    dplyr::select(-geometry) %>%
    rename(index_value = income.index) %>%
    mutate(City = "Barcelona", `Input Factor` = "Income Vulnerability"),
  # Bologna
  pop_bologna_df %>% 
    rename(index_value = pop.index) %>%
    mutate(City = "Bologna", `Input Factor` = "Population Density"),
  heat_index_bologna_df %>% 
    rename(index_value = heat_index_bologna) %>%
    mutate(City = "Bologna", `Input Factor` = "Heat Stress"),
  builddens_bologna_index_df %>% 
    rename(index_value = builddens_index_bologna) %>%
    mutate(City = "Bologna", `Input Factor` = "Building Density"),
  veg_vulnerability_index_bologna_df %>%
    rename(index_value = veg_index_vuln_bologna) %>%
    mutate(City = "Bologna", `Input Factor` = "Vegetation Cover Vulnerability"),
  age_bologna_index %>% 
    dplyr::select(-geom) %>%
    rename(index_value = age.index) %>%
    mutate(City = "Bologna", `Input Factor` = "Elderly Population"),
  income_bologna_index %>% 
    dplyr::select(-geometry) %>%
    rename(index_value = income.index) %>%
    mutate(City = "Bologna", `Input Factor` = "Income Vulnerability"),
  # Vienna
  pop_vienna_df %>% 
    rename(index_value = pop.index) %>%
    mutate(City = "Vienna", `Input Factor` = "Population Density"),
  heat_index_vienna_df %>% 
    rename(index_value = heat_index_vienna) %>%
    mutate(City = "Vienna", `Input Factor` = "Heat Stress"),
  builddens_vienna_index_df %>% 
    rename(index_value = builddens_index_vienna) %>%
    mutate(City = "Vienna", `Input Factor` = "Building Density"),
  veg_vulnerability_index_vienna_df %>%
    rename(index_value = veg_index_vuln_vienna) %>%
    mutate(City = "Vienna", `Input Factor` = "Vegetation Cover Vulnerability"),
  age_vienna_index %>% 
    dplyr::select(-geom) %>%
    rename(index_value = age.index) %>%
    mutate(City = "Vienna", `Input Factor` = "Elderly Population"),
  income_vienna_index %>% 
    dplyr::select(-geometry) %>%
    rename(index_value = income.index) %>%
    mutate(City = "Vienna", `Input Factor` = "Income Vulnerability")
 )

# Define Order
combined_input_data <- combined_input_data %>%
  mutate(`Input Factor` = factor(`Input Factor`, 
                                 levels = c("Population Density",
                                            "Heat Stress",
                                            "Building Density",
                                            "Vegetation Cover Vulnerability",
                                            "Elderly Population",
                                            "Income Vulnerability")))

# Create summary table grouped by source
datasummary(
  `Input Factor` * City ~ index_value * (Min + P25 + Median + P75 + Max + Mean),
  data = combined_input_data,
  output = "gt"
) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(3)
    ),
    locations = cells_column_labels()  # Top border above header
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(2) 
    ),
    locations = cells_body(
      rows = City == "Athens"  # Add border above each first city (Athens)
      
    )
  ) %>%
  gtsave("Data/combinedplots/input_factor_values_table.png")


#### HEAT RISK INDEX VALUE TABLE ####
combined_heatrisk_data <- bind_rows(
  athens_vulnerability_index_df %>% 
    rename(index_value = vulnerability_index) %>%
    mutate(City = "Athens"),
  barcelona_vulnerability_index_df %>% 
    rename(index_value = vulnerability_index) %>%
    mutate(City = "Barcelona"),
  bologna_vulnerability_index_df %>% 
    rename(index_value = vulnerability_index) %>%
    mutate(City = "Bologna"),
  vienna_vulnerability_index_df %>% 
    rename(index_value = vulnerability_index) %>%
    mutate(City = "Vienna")
)


# Create summary table grouped by source
datasummary(
  City ~ index_value * (Min + P25 + Median + P75 + Max + Mean),
  data = combined_heatrisk_data,
  output = "gt"
) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(3)
    ),
    locations = cells_column_labels()  # Top border above header
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(2) 
    ),
    locations = cells_body(
      rows = City == "Athens"  # Add border above each first city (Athens)
    )
  ) %>%
  gtsave("Data/combinedplots/heatrisk_values_table.png")


#### MAP SELECTED CITIES #####
install.packages(c("rnaturalearth","rnaturalearthdata","ggspatial","tmap"))

library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(tmap)

# data
countries <- ne_countries(scale = "medium", returnclass = "sf")

cities_df <- data.frame(
  name = c("Athens","Barcelona","Bologna","Vienna"),
  lon  = c(23.7275, 2.1734, 11.3426, 16.3738),
  lat  = c(37.9838,41.3851,44.4949,48.2082)
)
cities <- st_as_sf(cities_df, coords = c("lon","lat"), crs = 4326)

# project to ETRS89 / LAEA Europe for consistent distances
crs_target <- 3035
countries_p <- st_transform(countries, crs_target)
cities_p    <- st_transform(cities, crs_target)

# get projected coordinates for label placement
cities_lab <- cbind(st_drop_geometry(cities_p), st_coordinates(cities_p))
# cities_lab now has columns X and Y (projected easting/northing)

# bounding box
bb <- st_bbox(cities_p)
padx <- 700000
pady <- 250000
xlim <- c(bb["xmin"] - padx, bb["xmax"] + padx)
ylim <- c(bb["ymin"] - pady, bb["ymax"] + pady)

p <- ggplot() +
  geom_sf(data = countries_p, fill = "grey95", color = "grey70", size = 0.2) +
  # points using projected coords
  geom_point(data = cities_lab, aes(x = X, y = Y), size = 3) +
  # labels placed next to the points (adjust nudge_x/nudge_y if needed)
  geom_text_repel(data = cities_lab,
                  aes(x = X, y = Y, label = name),
                  inherit.aes = FALSE,
                  nudge_x = -100000,
                  nudge_y = 60000,         
                  force = 0.5,
                  point.padding = 0.5,
                  min.segment.length = 0.3,
                  size = 4) +
  coord_sf(crs = st_crs(3035), xlim = xlim, ylim = ylim, expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.15,"in"), pad_y = unit(0.4,"in"),
                         style = north_arrow_fancy_orienteering()) +
  labs(x = "Longitude", y = "Latitude") + 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = NA))

print(p)


ggsave("Data/combinedplots/map_selected_cities.png", p, width = 8, height = 4, dpi = 300)
