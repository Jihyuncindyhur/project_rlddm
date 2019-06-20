## plotting parameter recovery results
## model = hba_two_v

load("~/project_hur/sim_two_v_12sub_50.RData")
load("~/project_hur/true_param_two_v.RData")

library(ggplot2)
library(rstan)

colnames(true_param_two_v) <- c('tau_true', 'b1_true', 'b2_true', 'b3_true', 'w_true', 'alpha_true', 'omega_true')

tau_df   <- data.frame(summary(sim_two_v_12sub_50)$summary[8:19, ])
b1_df    <- data.frame(summary(sim_two_v_12sub_50)$summary[20:31, ])
b2_df    <- data.frame(summary(sim_two_v_12sub_50)$summary[32:43, ])
b3_df    <- data.frame(summary(sim_two_v_12sub_50)$summary[44:55, ])
w_df     <- data.frame(summary(sim_two_v_12sub_50)$summary[56:67, ])
alpha_df <- data.frame(summary(sim_two_v_12sub_50)$summary[68:79, ])
omega_df <- data.frame(summary(sim_two_v_12sub_50)$summary[80:91, ])

rownames(tau_df) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
rownames(b1_df) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
rownames(b2_df) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
rownames(b3_df) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
rownames(w_df) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
rownames(alpha_df) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
rownames(omega_df) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')

data_tau <- cbind(tau_df, tau_true = true_param[, 1])
data_b1  <- cbind(b1_df, b1_true = true_param[, 2])
data_b2  <- cbind(b2_df, b2_true = true_param[, 3])
data_b3  <- cbind(b3_df, b3_true = true_param[, 4])
data_w   <- cbind(w_df, w_true = true_param[, 5])
data_al  <- cbind(alpha_df, alpha_true = true_param[, 7])
data_om  <- cbind(omega_df, omega_true = true_param[, 8])


plot_tau <- ggplot(data_tau, aes(x=tau_true, y=mean)) + 
              geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.005) +
              geom_point(color = 'blue') +
              coord_cartesian(xlim =c(0, 0.15), ylim = c(0, 0.15)) +
              geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
              labs(y = 'estimated mean values', x = 'tau true parameter values') +
              stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
              ggtitle('tau')

plot_b1 <- ggplot(data_b1, aes(x=b1_true, y=mean)) + 
            geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.05) +
            geom_point(color = 'blue') +
            coord_cartesian(xlim =c(-0.5, 1.5), ylim = c(-.5, 1.5)) +
            geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
            labs(y = 'estimated mean values', x = 'b1 true parameter values') +
            stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
            ggtitle('b1')

plot_b2 <- ggplot(data_b2, aes(x=b2_true, y=mean)) + 
            geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.05) +
            geom_point(color = 'blue') +
            coord_cartesian(xlim =c(-.7, 1), ylim = c(-.7, 1)) +
            geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
            labs(y = 'estimated mean values', x = 'b2 true parameter values') +
            stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
            ggtitle('b2')

plot_b3 <- ggplot(data_b3, aes(x=b3_true, y=mean)) + 
            geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.05) +
            geom_point(color = 'blue') +
            coord_cartesian(xlim =c(-0.3, 3), ylim = c(-0.3, 3)) +
            geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
            labs(y = 'estimated mean values', x = 'b3 true parameter values') +
            stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
            ggtitle('b3')

plot_w <- ggplot(data_w, aes(x=w_true, y=mean)) + 
           geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.01) +
           geom_point(color = 'blue') +
           coord_cartesian(xlim =c(0.2, 0.65), ylim = c(0.2, 0.65)) +
           geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
           labs(y = 'estimated mean values', x = 'w true parameter values') +
           stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
           ggtitle('w')

plot_alpha <- ggplot(data_al, aes(x=alpha_true, y=mean)) + 
               geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0.02) +
               geom_point(color = 'blue') +
               coord_cartesian(xlim =c(0, 0.65), ylim = c(0, 0.65)) +
               geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
               labs(y = 'estimated mean values', x = 'alpha true parameter values') +
               stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
               ggtitle('alpha')

plot_omega <- ggplot(data_om, aes(x=omega_true, y=mean)) + 
               geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0.03) +
               geom_point(color = 'blue') +
               coord_cartesian(xlim =c(1.2, 2), ylim = c(1.2, 2)) +
               geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
               labs(y = 'estimated mean values', x = 'omega true parameter values') +
               stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
               ggtitle('omega')
