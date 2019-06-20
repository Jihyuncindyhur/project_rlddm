## plotting parameter recovery results
## model = hba_two_s

load("~/project_hur/sim_two_s_6sub_100_2.RData")
load("~/project_hur/true_param_two_s.RData")

library(ggplot2)
library(rstan)

colnames(true_param) <- c('tau_true', 'b0_true', 'b1_true', 'w1_true', 'w2_true', 'alpha_true', 'omega_true')

tau_df   <- data.frame(summary(sim_two_s_6sub_100_2)$summary[8:13, ])
b0_df    <- data.frame(summary(sim_two_s_6sub_100_2)$summary[14:19, ])
b1_df    <- data.frame(summary(sim_two_s_6sub_100_2)$summary[20:25, ])
w1_df    <- data.frame(summary(sim_two_s_6sub_100_2)$summary[26:31, ])
w2_df    <- data.frame(summary(sim_two_s_6sub_100_2)$summary[32:37, ])
alpha_df <- data.frame(summary(sim_two_s_6sub_100_2)$summary[38:43, ])
omega_df <- data.frame(summary(sim_two_s_6sub_100_2)$summary[44:49, ])

rownames(tau_df) <- c('1', '2', '3', '4', '5', '6')
rownames(b0_df) <- c('1', '2', '3', '4', '5', '6')
rownames(b1_df) <- c('1', '2', '3', '4', '5', '6')
rownames(w1_df) <- c('1', '2', '3', '4', '5', '6')
rownames(w2_df) <- c('1', '2', '3', '4', '5', '6')
rownames(alpha_df) <- c('1', '2', '3', '4', '5', '6')
rownames(omega_df) <- c('1', '2', '3', '4', '5', '6')

data_tau <- cbind(tau_df, tau_true = true_param[, 1])
data_b0 <- cbind(b0_df, b0_true = true_param[, 2])
data_b1 <- cbind(b1_df, b1_true = true_param[, 3])
data_w1 <- cbind(w1_df, w1_true = true_param[, 4])
data_w2 <- cbind(w2_df, w2_true = true_param[, 5])
data_al <- cbind(alpha_df, alpha_true = true_param[, 6])
data_om <- cbind(omega_df, omega_true = true_param[, 7])
data_tau

plot_tau <- ggplot(data_tau, aes(x=tau_true, y=mean)) + 
             geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.005) +
             geom_point(color = 'blue') +
             coord_cartesian(xlim =c(0, 0.15), ylim = c(0, 0.15)) +
             geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
             labs(y = 'estimated mean values', x = 'tau true parameter values') +
             stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
             ggtitle('tau')

plot_b0 <- ggplot(data_b0, aes(x=b0_true, y=mean)) + 
            geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.035) +
            geom_point(color = 'blue') +
            coord_cartesian(xlim =c(0, 1), ylim = c(0, 1)) +
            geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
            labs(y = 'estimated mean values', x = 'b0 true parameter values') +
            stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
            ggtitle('b0')

plot_b1 <- ggplot(data_b1, aes(x=b1_true, y=mean)) + 
            geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.05) +
            geom_point(color = 'blue') +
            coord_cartesian(xlim =c(0.3, 3), ylim = c(0.3, 3)) +
            geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
            labs(y = 'estimated mean values', x = 'b1 true parameter values') +
            stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
            ggtitle('b1')

plot_w1 <- ggplot(data_w1, aes(x=w1_true, y=mean)) + 
            geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.01) +
            geom_point(color = 'blue') +
            coord_cartesian(xlim =c(0.2, 0.5), ylim = c(0.2, 0.5)) +
            geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
            labs(y = 'estimated mean values', x = 'w1 true parameter values') +
            stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
            ggtitle('w1')

plot_w2 <- ggplot(data_w2, aes(x=w2_true, y=mean)) + 
            geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.01) +
            geom_point(color = 'blue') +
            coord_cartesian(xlim =c(0.2, 0.5), ylim = c(0.2, 0.5)) +
            geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
            labs(y = 'estimated mean values', x = 'w2 true parameter values') +
            stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
            ggtitle('w2')

plot_alpha <- ggplot(data_al, aes(x=alpha_true, y=mean)) + 
               geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0.025) +
               geom_point(color = 'blue') +
               coord_cartesian(xlim =c(0, 0.5), ylim = c(0, 0.5)) +
               geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
               labs(y = 'estimated mean values', x = 'alpha true parameter values') +
               stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
               ggtitle('alpha')

plot_omega <- ggplot(data_om, aes(x=omega_true, y=mean)) + 
               geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0.03) +
               geom_point(color = 'blue') +
               coord_cartesian(xlim =c(1.2, 1.9), ylim = c(1.2, 1.9)) +
               geom_abline(intercept = 0, slope = 1, color = 'darkblue') +
               labs(y = 'estimated mean values', x = 'omega true parameter values') +
               stat_smooth(method = "lm", se = FALSE, col = "darkblue", linetype = 'dotted') +
               ggtitle('omega')
