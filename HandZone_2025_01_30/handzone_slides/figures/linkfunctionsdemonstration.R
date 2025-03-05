
######################################

rm(list=ls())
library(ggplot2)

######################################

# IDENTITY

x_vals = seq(-4, 4, length.out = 300)
y_vals = x_vals
data = data.frame(x = x_vals, y = y_vals)

# Define x positions for vertical lines (equal intervals in x)
x_positions = seq(-4,4,1)
y_positions = x_positions

# plot
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", linewidth = 2) +  # Use linewidth instead of size
  geom_vline(xintercept = x_positions, linetype = "dashed", color = "red", linewidth=.8) +
  geom_hline(yintercept = y_positions, linetype = 1, color = "#777777", linewidth=1) +
  labs(subtitle = "Equal intervals on X correspond to equal intervals on Y",
       x = "X values (linear predictor)",
       y = "Y values (response)") +
  theme_minimal() +
  scale_x_continuous(breaks=-10:10,limits=c(-5,4.3))+
  theme(text=element_text(size=18)) + 
  geom_text(data = data.frame(x = x_positions, y = -3.5, 
                              label = paste0("x = ", x_positions)), 
            aes(x = x, y = y, label = label), 
            color = "black", vjust = 3, size=4, fontface="bold") +
  geom_text(data = data.frame(x = -5, y = y_positions, 
                              label = paste0("y = ", round(y_positions, 2))), 
            aes(x = x, y = y, label = label), 
            color = "black", hjust = 0, vjust=-.2, size=4, fontface="bold")

######################################

# LOG

x_vals = seq(-2, 2, length.out = 300)
y_vals = exp(x_vals)
data = data.frame(x = x_vals, y = y_vals)

# Define x positions for vertical lines (equal intervals in x)
x_positions = seq(-2,2,.5)
y_positions = exp(x_positions)

# plot
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", linewidth = 2) +  # Use linewidth instead of size
  geom_vline(xintercept = x_positions, linetype = "dashed", color = "red", linewidth=.8) +
  geom_hline(yintercept = y_positions, linetype = 1, color = "#777777", linewidth=1) +
  labs(subtitle = "Equal intervals on X do NOT correspond to equal intervals on Y",
       x = "X values (linear predictor)",
       y = "Y values (response)") +
  theme_minimal() +
  scale_x_continuous(breaks=-10:10,limits=c(-2.7,2.2))+
  theme(text=element_text(size=18)) + 
  geom_text(data = data.frame(x = x_positions, y = 0.5, 
                              label = paste0("x = ", x_positions)), 
            aes(x = x, y = y, label = label), 
            color = "black", vjust = 3, size=4, fontface="bold") +
  geom_text(data = data.frame(x = -2.7, y = y_positions, 
                              label = paste0("y = ", round(y_positions, 2))), 
            aes(x = x, y = y, label = label), 
            color = "black", hjust = 0, vjust=-.2, size=4, fontface="bold")

######################################

# LOGIT

# Generate data for plotting
x_vals = seq(-6, 6, length.out = 300)
Y_Logit = plogis(x_vals)
data = data.frame(x = x_vals, Y_Logit = Y_Logit)

# Define x positions for vertical lines (equal intervals in x)
x_positions = seq(-4,4,1)

# Compute corresponding y values
y_logis = plogis(x_positions)

# PLOT
ggplot(data, aes(x = x, y = Y_Logit)) +
  geom_line(color = "blue", linewidth = 2) +  # Use linewidth instead of size
  geom_vline(xintercept = x_positions, linetype = "dashed", color = "red", linewidth=.8) +
  geom_hline(yintercept = y_logis, linetype = 1, color = "#777777", linewidth=1) +
  labs(subtitle = "Equal intervals on X do NOT correspond to equal intervals on Y",
       x = "X values (linear predictor)",
       y = "Probability / Accuracy") +
  theme_minimal() +
  scale_x_continuous(breaks=-10:10,limits=c(-6,4.3))+
  theme(text=element_text(size=18)) + 
  geom_text(data = data.frame(x = x_positions, y = 0.05, 
                              label = paste0("x = ", x_positions)), 
            aes(x = x, y = y, label = label), 
            color = "black", vjust = 3, size=4, fontface="bold") +
  geom_text(data = data.frame(x = -6, y = y_logis, 
                              label = paste0("y = ", round(y_logis, 2))), 
            aes(x = x, y = y, label = label), 
            color = "black", hjust = 0, vjust=-.2, size=4, fontface="bold")

######################################

# PROBIT

# Generate data for plotting
x_vals = seq(-6, 6, length.out = 300)
Y_Probit = pnorm(x_vals)
data = data.frame(x = x_vals, Y_Probit = Y_Probit)

# Define x positions for vertical lines (equal intervals in x)
x_positions = seq(-4,4,1)

# Compute corresponding y values
y_probit = pnorm(x_positions)

# PLOT
ggplot(data, aes(x = x, y = Y_Probit)) +
  geom_line(color = "blue", linewidth = 2) +  # Use linewidth instead of size
  geom_vline(xintercept = x_positions, linetype = "dashed", color = "red", linewidth=.8) +
  geom_hline(yintercept = y_probit, linetype = 1, color = "#777777", linewidth=1) +
  labs(subtitle = "Equal intervals on X do NOT correspond to equal intervals on Y",
       x = "X values (linear predictor)",
       y = "Probability / Accuracy") +
  theme_minimal() +
  scale_x_continuous(breaks=-10:10,limits=c(-6,4.3))+
  theme(text=element_text(size=18)) + 
  geom_text(data = data.frame(x = x_positions, y = 0.05, 
                              label = paste0("x = ", x_positions)), 
            aes(x = x, y = y, label = label), 
            color = "black", vjust = 3, size=4, fontface="bold") +
  geom_text(data = data.frame(x = -6, y = y_probit, 
                              label = paste0("y = ", round(y_probit, 2))), 
            aes(x = x, y = y, label = label), 
            color = "black", hjust = 0, vjust=-.2, size=4, fontface="bold")


######################################


