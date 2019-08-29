## Scatterplot matrices of model variables for supplementary materials

## packages
library('ggplot2')
library('ggforce')
library('readr')
library('here')
library('dplyr')

## load in the model data
pond <- read_csv(here('ModelData.csv'))

## co2 figure
## needs DO.sat, Alk.mg.L, NOx.ug.N.L, b.f.max, CEC, delI18O, Residence_time,
##       Elevation, Salinity_code, Landuse
carbon <- select(pond, CO2.uM, DO.sat, Alk.mg.L, NOx.ug.N.L, b.f.max, CEC,
                 delI18O, Residence_time, Elevation, Salinity_code, Landuse)
carbon <- mutate(carbon, CO2.uM = log(CO2.uM), DO.sat = sqrt(DO.sat),
                 Alk.mg.L = log(Alk.mg.L), NOx.ug.N.L = log(NOx.ug.N.L),
                 b.f.max = sqrt(b.f.max), CEC = log(CEC),
                 delI18O, Residence_time = log(Residence_time),
                 Elevation = log(Elevation))
carbon <- carbon[complete.cases(carbon), ]
carbon_names <- c('log(CO[2])', 'sqrt(O[2])', 'log(Alk)', 'log(NO[x])',
                  'sqrt(BF)', 'log(CEC)', 'delta[i]^{18}*O', 'log(RT)',
                  'log(Elev)', 'Soil Salinity', 'Land use')
carbon <- rename_at(carbon, vars(CO2.uM:Landuse), ~ carbon_names)
## ch4 figure
## needs DO.sat, SedimentCN, DIN.ug.N.L., Conductivity, b.f.max, delI18O,
##       Residence_time, KSAT, Elevation, Landuse
methane <- select(pond, CH4.uM, DO.sat, SedimentCN, DIN.ug.N.L, Conductivity,
                  b.f.max, delI18O, Residence_time, KSAT, Elevation, Landuse)
methane <- mutate(methane, CH4.uM = log(CH4.uM), DO.sat = sqrt(DO.sat),
                 DIN.ug.N.L = log(DIN.ug.N.L), Conductivity = log(Conductivity),
                 b.f.max = sqrt(b.f.max),
                 delI18O, Residence_time = log(Residence_time),
                 KSAT = log(KSAT), Elevation = log(Elevation))
methane <- methane[complete.cases(methane), ]
methane_names <- c('log(CH[4])', 'sqrt(O[2])', 'C:N', 'log(DIN)',
                  'log(Conductivity)', 'sqrt(BF)', 'delta[i]^{18}*O', 'log(RT)',
                  'log(K[sat])', 'log(Elev)', 'Land use')
methane <- rename_at(methane, vars(CH4.uM:Landuse), ~ methane_names)

## carbon plot
co2_plt <- ggplot(carbon) +
    geom_autopoint(alpha = 0.4, size = 1) +
    geom_autodensity(aes(fill = `Land use`), alpha = 0.75) +
    facet_matrix(vars(`log(CO[2])`:`Land use`), layer.diag = 2,
                 alternate.axes = TRUE) +
    theme(legend.position = 'top',
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 10),
          axis.text = element_text(size = 7)) +
    scale_fill_viridis_d()

ggsave(here('figures', 'co2_splom.pdf'), co2_plt, height = 10, width = 15)

## methane plot
ch4_plt <- ggplot(methane) +
    geom_autopoint(alpha = 0.4, size = 1) +
    geom_autodensity(aes(fill = `Land use`), alpha = 0.75) +
    facet_matrix(vars(`log(CH[4])`:`Land use`), layer.diag = 2,
                 alternate.axes = TRUE) +
    theme(legend.position = 'top',
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 10),
          axis.text = element_text(size = 7)) +
    scale_fill_viridis_d()

ggsave(here('figures', 'ch4_splom.pdf'), ch4_plt, height = 10, width = 15)
