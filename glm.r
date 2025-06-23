## Load relevant libraries
library(SPLICE)
library(DT)
library(data.table)
library(ggplot2)
library(patchwork)

## Generate dataset
data <- generate_data(
    n_claims_per_period = 2500,                 # expected claims per period param
    n_periods = 45,                             # number of accident & development periods
    complexity = 2,                             # scen. 2
    random_seed = 21
)


## Init dataframes & write to csv
# paids = data$payment_dataset
# incurreds = data$incurred_dataset
# claims = data$claim_dataset
# write.csv(paids, "paids_simdata.csv")
# write.csv(incurreds, "inc_simdata.csv")
# write.csv(claims, "claims_simdata.csv")

# write to RDS then read datasets
saveRDS(data, file="_simdata_ex.RDS")      # Save dataset
data = readRDS("_simdata_ex.RDS")

## Simplify to recognizable triangle dataset

# Strip paid dataframe & create dev. period variable
paids_data = data$payment_dataset
dev_year = paids_data$payment_period - paids_data$occurrence_period + 1

# Attach dev. period variable & sum over individual payments to generate aggregated triangles
paids_data = cbind(paids_data, dev_year)
paids_data = aggregate(paids_data$payment_inflated, list(paids_data$occurrence_period, paids_data$dev_year), FUN = sum)

# Strip out future payments
colnames(paids_data) = c("acc_year", "dev_year", "payments")
paids_data = paids_data[paids_data$dev_year < 46]

# Add zero increments
zero_vec = matrix(c(0), ncol = 2, nrow = 45)     # create 2x45 matrix
colnames(zero_vec) = c("acc_year", "dev_year")

zero_vec_a = NULL

for (i in 1:45) {
    zero_vec[,1] = i                                # Set first column as i
    zero_vec[,2] = c(1:45)                          # Set second column to 1:45 (dev years)
    zero_vec_a = rbind(zero_vec_a, zero_vec)        # Combine matrices to obtain dev year x accident year
}

# Set payments to 0
zero_vec_b = cbind(c(0), zero_vec_a)
colnames(zero_vec_b) = c("payments", "acc_year", "dev_year")
paids_data = rbind(paids_data, zero_vec_b)

# 0 increments in null triangle points
paids_data = aggregate(paids_data$payments, list(paids_data$acc_year, paids_data$dev_year), FUN = sum)
colnames(paids_data) = c("acc_year", "dev_year", "payments")

# Add future/past attribute to determine whether a data point falls into the training set
for (i in c(1:length(paids_data$payments))) {
    # Future data points = FALSE & Past = TRUE
    if ((paids_data$acc_year[i] + paids_data$dev_year[i]) < 47) {paids_data$train_ind[i] = TRUE} else {paids_data$train_ind[i] = FALSE}
}

# Strip out future periods (> period 45)
paids_data = paids_data[paids_data$dev_year < 46,]

datatable(paids_data) |> formatRound("payments", digits = 0)

# First plot - Incremental development

plot1 <- ggplot(data=paids_data, aes(x=dev_year, y=payments, colour=as.factor(acc_year))) +
  geom_line(linewidth = 1) + 
  scale_color_viridis_d(begin=0.9, end=0) +  
  ggtitle("Incremental") +  
  theme_bw() +  
  theme(legend.position = "none", legend.title=element_blank(), legend.text=element_text(size=8)) 
plot1

cum_paids = NULL

for (i in 1 : 45) {
  o = paids_data$payments[paids_data$acc_year == i]
  o = cumsum(o)
  o = data.frame(acc_year = i, dev_year = 1:(45) , cumulative = o)
  cum_paids = rbind(cum_paids , o)
}

# Second plot - Cumulative development

plot2 <- ggplot(data=cum_paids, aes(x=dev_year, y=cumulative, colour=as.factor(acc_year))) +  # Plot 
  
  geom_line(linewidth=1) + 
  scale_color_viridis_d(begin=0.9, end=0) +  
  ggtitle("Cumulative") +  
  theme_bw() +  
  theme(legend.position = "bottom", legend.title=element_blank(), legend.text=element_text(size=6)) 

plot2

## Reserving with GLMs

library(data.table)
library(patchwork)

# Adjust formatting (remove scientific notation in plot)
options(scipen = 99)

# Model fitting (cl_yr - calendar year factor for diagnostics)
paids_GLM = data.table(paids_data)
paids_GLM = paids_GLM[paids_GLM$train_ind == TRUE]  # Use training dataset ONLY

paids_GLM[, accf := as.factor(acc_year)
][, devf := as.factor(dev_year)   
][, cl_yr := acc_year + dev_year - 1] 

glm_fit_a <- glm(data = paids_GLM,  
                family = quasipoisson(link = "log"), 
                formula = "payments ~ 0 + accf + devf") 


glm_fit_a$coeff_table <- data.table(parameter = names(glm_fit_a$coefficients),  
                                    coeff_glm_fit_a = glm_fit_a$coefficients) 

head(glm_fit_a$coeff_table)   # Return head records

# Create null dataset of future increments
ay <- NULL 
dy <- NULL 

# Apply GLM prediction to generate reserve estimate
for(i in 2:45){ 
  
  ay <- c(ay, rep(i, times=(i-1))) 
  dy <- c(dy, (45-i+2):45) 
  
} 

f_data <- data.table(acc_year = ay, dev_year = dy) 

# Factors
f_data[, cl_yr := acc_year + dev_year 
        ][, accf := as.factor(acc_year) 
            ][, devf := as.factor(dev_year)] 

# Prediction and sum by accident year
x <- predict(glm_fit_a, newdata = f_data, type="response") 

f_data[, incremental := x] 

# Summary by accident year (using data.table syntax)
o_year <- f_data[,  lapply(.SD, sum), .SDcols=c("incremental"), by="acc_year"] 

# Totals
o_total <- f_data[, sum(incremental)] 

# Print accident year table (including totals)
o_year[, acc_year := as.character(acc_year) ]
o_year_print <- rbind(o_year, data.table(acc_year="Total", incremental=o_total)) 
setnames(o_year_print, "incremental", "OCL")
datatable(o_year_print)|> formatRound("OCL", digits = 0) 


# Check reserve matches chain ladder method

# Poll cumulative payments to calculate CL factors
paids_GLM[, cumpmts := cumsum(payments), by=.(acc_year)][train_ind==FALSE, cumpmts := NA] 

cl_factor <- numeric(44) # hold CL factors for no. of years for which reserves are required

for (j in 1:44){  # Weighted avg. on all years
  cl_factor[j] <- 
    paids_GLM[train_ind==TRUE & dev_year == (j+1) & acc_year <= (45-j), sum(cumpmts)] / 
    paids_GLM[train_ind==TRUE & dev_year == (j) & acc_year <= (45-j), sum(cumpmts)] 
} 

# Accumulate CL factors 
cl_cmlt <- rev(cumprod(rev(cl_factor))) 

# Leading diag. for projections
lead_diag <- paids_GLM[train_ind==TRUE & acc_year + dev_year == 46 & acc_year > 1, cumpmts] 

# Current CL amounts
cl_os <- cl_cmlt * lead_diag - lead_diag

sum(cl_os) - o_year_print$OCL[45]


# Print results by accident year

cl_os = rev(cl_os)
cl_os[45] = sum(cl_os)

o_year_print$cl_os = cl_os

o_year_print$difference = o_year_print$cl_os - o_year_print$OCL

datatable(o_year_print )|> formatRound(c("OCL", "cl_os"), digits = 0) |> formatRound("difference", digits = 4) 