rm(list = ls())
pkgs <- c("ggplot2", "dplyr", "gridExtra", "rsm", "directlabels")
sapply(pkgs, require, character.only = T)
setwd("C:\\Users\\User\\Dropbox\\Unicamp\\MI404 - Métodos Estatísticos\\Trabalhos\\Superfície de Resposta")
FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}


# Leitura dados -----------------------------------------------------------

motile_surv <- read.csv("dados.csv" )

motile_surv_coded <- as.coded.data(motile_surv,
                                   x1 ~ (sodium_citrate_pct - 3)/0.7,
                                   x2 ~ (glycerol - 8)/3,
                                   x3 ~ (hours_before_freezing - 16)/6)

so_model_x1_x2 <- rsm(y ~ SO(x1,x2),data = motile_surv_coded)


# 3D figure ---------------------------------------------------------------
xs    <- canonical(so_model_x1_x2)$xs
col_f <- colorRampPalette(c("dodgerblue2", "white", "firebrick2"))

pdf(file = "plot3D.pdf", width = 10, height = 7, family = "Palatino")
par(mar = c(1.5, 1.5, 0.5, 0.5), cex = 1.6)
persp(so_model_x1_x2, ~ x1 + x2, at = xs, col = col_f(30), contours = "colors",
      border = 'black', ticktype = "detailed", 
      expand = 0.8,
      xlab = c("% de citrato de sódio", "% de glicerol"),
      zlab = "")
graphics.off()


# Contour plot ------------------------------------------------------------
aux <- contour(so_model_x1_x2, ~ x1 + x2, image = FALSE, at = xs, plot.it = F)
x <- aux$`x1 ~ x2`$x
y <- aux$`x1 ~ x2`$y
z <- as.vector(aux$`x1 ~ x2`$z)
h <- expand.grid(x = x, y = y)
df <- cbind(h, z = z)


df_new <- data.frame(x1 = xs[1], x2 = xs[2]) %>% 
  mutate(x = x1 * 0.7 + 3,
         y = x2 * 3 + 8)
df_new$z <- round(predict(so_model_x1_x2, newdata = df_new))

p1 <- ggplot(df, aes(x = x, y = y, z = z, fill = z)) + 
  geom_raster() +
  geom_contour(aes(colour = ..level..), col = "black", size = 1.0) + 
  geom_point(data = df_new, aes(x = x, y = y), col = "black", fill = "firebrick2", shape = 21, size = 3.0) +
  geom_text(data = df_new, aes(x = x, y = y, label = z), vjust = -0.5, size = 5.0) +
  scale_fill_gradient2(low ="dodgerblue2", high ="firebrick2", mid = "white", midpoint = quantile(df$z, 0.25),
                       limits = range(df$z)) +
  scale_x_continuous(breaks = seq(min(df$x), max(df$x), l = 5), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(min(df$y), max(df$y), l = 5), expand = c(0,0)) +
  labs(x = "% de citrato de sódio", y = "% de glicerol") +
  theme_bw() +
  theme(text             = element_text(family = "Palatino", colour = "black", size = 22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
direct.label(p1,  list("bottom.pieces", colour = "black", cex = 1.2, vjust = -0.3))
ggsave(filename = "contorno.pdf", device = "pdf", width = 10, height = 7)



# Residuals plots ---------------------------------------------------------
resid_df <- tibble(ajustado = fitted(so_model_x1_x2), 
                   residuo = resid(so_model_x1_x2),
                   std_resid = rstudent(so_model_x1_x2)) 

p1 <- resid_df %>% 
  ggplot(aes(ajustado, std_resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "black") +
  geom_hline(yintercept = c(-2,2), col = "red", lty = 2) +
  labs(x = "Valores preditos", y = "Resíduos studentizados") +
  theme(text = element_text(size = 16), panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 1.2))

p2 <- resid_df %>% 
  ggplot(aes(sample = std_resid)) + 
  geom_qq(size = 2.0, pch = 21, fill = "black", col = "blue") + 
  geom_qq_line(size = 1.0) + 
  labs(x="Quantis teóricos da N(0,1)", y="Resíduos studentizados") +
  theme(text = element_text(size = 16), panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 1.0))


ggsave(filename = "res1.pdf", plot = p1, device = "pdf", width = 9, height = 7.5)
ggsave(filename = "res2.pdf", plot = p2, device = "pdf", width = 9, height = 7.5)

grid.arrange(p1,p2)





