###------Figure 1 -----
## @knitr DiceFigure

source("./R/SCRIPTS/000-Libraries.R")

## Loading in the Projections from Scott Kulp and Climate Central
files <- paste0("R/DATA/Dat/", list.files(path = "./R/DATA/Dat/",pattern = 'rcp45\\.csv'))
files <- files[files != "R/DATA/Dat/BlockgroupProjectedEAE.v3.LA.rcp45.csv" ]
files2 <- read_csv("./R/DATA/Dat/BlockgroupProjectedEAE.v3.LA.rcp45.csv") %>%
  pivot_longer(-GEOID10, names_to = "variable", values_to = "count") %>%
  group_by(variable) %>%
  dplyr::summarise(Population = sum(count)) %>%
  separate(variable, c("variable", "year", "prob", "SSP")) %>%
  filter(variable != "LECZ")
temp <- lapply(files, read_csv)

## Converting the Climate Central data into a useable format.
### Also summing to the national level
together <- rbindlist( temp ) %>%
  pivot_longer(-GEOID10, names_to = "variable", values_to = "count") %>%
    group_by(variable) %>%
  dplyr::summarise(Population = sum(count)) %>%
  separate(variable, c("variable", "year", "prob", "SSP")) %>%
  rbind(., files2) %>%
  group_by(variable, year, prob, SSP) %>%
  dplyr::summarise(Population = sum(Population)) %>%
  mutate(SSP2 = if_else(is.na(SSP), prob, SSP),
         prob2 = if_else(is.na(SSP),"p50", prob),
         Type = case_when(
           variable == "EAE" ~ "Exp. Ann. Flood",
           variable == "MHHW" ~ "Inundated",
           variable == "RL100" ~ "100-year FP"
         ),
         year = as.numeric(year))  %>%
  ungroup() %>%
  dplyr::select(everything(), -variable, -prob, -SSP ) %>%
  pivot_wider( names_from = Type, values_from = Population) %>%
  mutate(`Exp. Ann. Flood` = if_else(prob2 == "p50", (`Exp. Ann. Flood` - Inundated), `Exp. Ann. Flood`),
         `100-year FP` = if_else(prob2 == "p50", (`100-year FP` - `Exp. Ann. Flood` - Inundated), `100-year FP`)) %>%
  pivot_longer(-c(year, SSP2, prob2), names_to = "Type", values_to = "Population") %>%
  mutate(Type = case_when(
    Type == "Exp. Ann. Flood" ~ "EAE",
    Type == "Inundated" ~ "Specified SLR (MHHW)",
    Type == "100-year FP" ~ "Coastal Floodplains (RL100)"
  ))

together$Type <- factor(together$Type, levels = c("Specified SLR (MHHW)", "EAE", "Coastal Floodplains (RL100)"))



this.ssp <- "SSP2"
scen <- "Fossil-fueled development"

plotfunc <- function(this.ssp, scen){
  
  dat <- together %>%
    # filter(SSP2 == this.ssp)
    filter(SSP2 == this.ssp, prob2 == "p50")
 
  h1 <- sum(filter(dat, year == "2100")$Population)
  
  h2 <- max(filter(dat, Type == "Specified SLR (MHHW)")$Population)
  h3 <- max(filter(dat, Type == "EAE")$Population)+ 
    h2
  
  dat2 <- together %>%
    filter(SSP2 == this.ssp) 
  
  lowera <- dat2 %>%
    group_by(year, Type, prob2) %>%
    filter(prob2 == "p5") %>%
    ungroup() %>%
    dplyr::select(year, Type, lower = Population)
  
  uppera <- dat2 %>%
    group_by(year, Type, prob2) %>%
    filter(prob2 == "p95") %>%
    ungroup() %>%
    dplyr::select(year, Type, upper = Population)
  
  dat2 <- left_join(dat2, lowera) %>%
    left_join(., uppera) %>%
    filter(prob2  %in% c("a", "p50")) %>%
    arrange(Type) %>%
    mutate(lower2 = if_else(Type == "Specified SLR (MHHW)", lower, lag(Population, 11) + lower),
           upper2 = if_else(Type == "Specified SLR (MHHW)", upper, lag(Population, 11) + upper))

 
  
  ggplot(data = dat2, aes(y=Population, x = year, fill = reorder(Type, desc(Type))), alpha = 0.5) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
    scale_y_continuous(labels=function(x) format(x/1000000), 
                       expand=c(0,0), 
                       limits = c(0, max(na.omit(together$Population)*1.1)),
                       breaks = c(seq(0,max(na.omit(together$Population)*1.1), 5000000)),
                       sec.axis = sec_axis(~.,breaks = c(h1,h2,h3),labels=function(x) format(round(x/1000000, digits =1))),
                       # formatter = function(x) format(x*10000)
    ) +
    scale_x_continuous(expand=c(0,0)) +
    scale_fill_manual(values=c("dark gray", "blue", "red")) +
    # geom_hline(yintercept = h1) +
    # geom_hline(yintercept = h2) +
    # geom_text(aes(2025, h, label = paste0(prettyNum(h,big.mark=",",scientific=FALSE)),
    #                vjust = -1), size = 3) +
    guides(fill=guide_legend(title="Exposure Cat.")) +
    labs(x = "Year",
         y = "Pop. Exposure (mill)",
         # y = "",
         title = paste0(this.ssp,": ", scen)) +
    theme_bw() +
    NULL}
plotfunc("SSP4", "Middle of the Road")

ssp1 <- plotfunc("SSP1", "Sustainability")
ssp2 <- plotfunc("SSP2", "Middle of the road")
ssp3 <- plotfunc("SSP3", "Regional rivalry")
ssp4 <- plotfunc("SSP4", "Inequality")
ssp5 <- plotfunc("SSP5", "Fossil-fueled development")

leg <- get_legend(ssp1)

top <- plot_grid(ssp5 + theme(legend.position="none"),
                 ssp3 + theme(legend.position="none"),
                 ncol=2, align = 'h')
bot <- plot_grid(ssp1 + theme(legend.position="none"),
                 ssp4 + theme(legend.position="none"),
                 ncol=2, align = 'h')
mid <- plot_grid(leg, ssp2 + theme(legend.position="none"), NULL, ncol=3,rel_widths = c(1,2,1),
                 rel_heights = c(1,5,1))

maps<- plot_grid(top,
                 mid,
                 bot,
                 ncol=1, align = 'h', 
                 # rel_heights = c(2, 1),
                 label_x = 0.3)

fig<- ggplot() + 
  geom_blank() + 
  xlim(0, 10) + 
  ylim(0, 10) +
  theme_classic() + 
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y  = element_line(arrow = arrow()),
        axis.line.x = element_line(arrow = arrow())) +
  labs(x ="Barriers to Adaptation", 
       y = "Barriers to Mitigation") 

countymaps <- ggdraw() +
  draw_plot(fig) +
  draw_plot(maps, scale = 0.9)

ggsave("FIGURES/exposure2102021.pdf", width = 10.88, height = 7.86, units = "in")
ggsave("FIGURES/exposure2102021.png", width = 10.88, height = 7.86, units = "in")