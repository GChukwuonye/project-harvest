predict.dat <- ggeffect(model = Pb7,
                        terms = c("distance.rounded.km"),
                        back.transform = F,
                        type = "re")

ggplot(data = dat, aes(x = distance.rounded.km, y = log(Pb)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "",
       y = "ln(Pb) [µg/L]\n",
       x = "\n Distance From Smelter (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
s