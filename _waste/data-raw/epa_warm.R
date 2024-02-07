source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
# Dis-aggregate by each GHG and see if using AR5 yields significantly 
# different emission factors
# https://www.epa.gov/sites/default/files/2019-10/documents/warm_v15_management_practices_updated_10-08-2019.pdf
# Combustion  -----
# they use N2O GWP of 298
# 0.04 MTCO2E of N2O emissions per ton of mixed MSW combusted.
# MSW
0.04/298

# original value
0.39 + (0.0001342282*298)
round(0.39 + (0.0001342282*gwp$n2o), 2)

# Mixed organics
0.01 + (0.0001342282*298)
round(0.01 + (0.0001342282*gwp$n2o), 2)

0.03/298
# mixed recyclables
0.08 + (0.0001006711 * 298)
round(0.08 + (0.0001006711 * gwp$n2o), 2)

# result: no, not significantly different
