
a <- tribble(
  ~'County', 	~'FIPS', 	~'group', 	~'MHHW', 	~'EAE', 	~'RL100', 	~'LECZ', 
  'Poquoson city VA', 	51735, 	1, 	0.154721424724856, 	0.59381160351041, 	0.959859852249703, 	1.0001125806334, 
  'Dare NC', 	37055, 	1, 	0.19199600627502, 	0.404411694051093, 	0.587835749494261, 	0.985009343186192, 
  'Mathews VA', 	51115, 	2, 	0.0917180134679499, 	0.273404253183475, 	0.422136166915286, 	0.968944673282237, 
  'Somerset MD', 	24039, 	2, 	0.202850450281884, 	0.281155175096101, 	0.354267060264745, 	0.70853412052949, 
  'Accomack VA', 	51001, 	3, 	0.118246751769937, 	0.211139695502717, 	0.26775964916553, 	0.750227702107915, 
  'Wakulla FL', 	12129, 	3, 	0.00706457423882703, 	0.0450039544103055, 	0.240457175017853, 	0.841774546494309, 
  'Currituck NC', 	37053, 	4, 	0.23009382010674, 	0.451755475788329, 	0.647259288065014, 	0.99986883053181, 
  'Orange TX', 	48361, 	4, 	0.0219953730157703, 	0.0693892195654806, 	0.232443109059385, 	0.999751526559506, 
  'Franklin FL', 	12037, 	5, 	0.024980617864401, 	0.122020710337651, 	0.604201537577435, 	0.953106650826377, 
  'Grays Harbor WA', 	53027, 	5, 	0.0211292313527953, 	0.123885371670341, 	0.174043212662843, 	0.388835657220363, 
  'Norfolk city VA', 	51710, 	6, 	0.0136963517943916, 	0.101217269562677, 	0.274277757234082, 	0.997610927523462, 
  'Hancock MS', 	28045, 	6, 	0.0600327940230745, 	0.124356740831913, 	0.294355743112418, 	0.639598489967139, 
  'Gulf FL', 	12045, 	7, 	0.00657820927355533, 	0.0522550708490874, 	0.366434051787483, 	0.746951030470466, 
  'Somerset MD', 	24039, 	7, 	0.202850450281884, 	0.281155175096101, 	0.354267060264745, 	0.70853412052949, 
  'Pasquotank NC', 	37139, 	8, 	0.109418069064499, 	0.364969622312165, 	0.644321042771408, 	0.999939348498026, 
  'Glynn GA', 	13127, 	8, 	0.111548335123768, 	0.177742065298687, 	0.240772195683019, 	0.998110896621761, 
  'Chatham GA', 	13051, 	9, 	0.0507995660158517, 	0.117493264621963, 	0.151961951947283, 	0.984771681431769, 
  'San Mateo CA', 	06081, 	9, 	0.076528478534058, 	0.123373681628737, 	0.151897231379301, 	0.377064304975427, 
  'McIntosh GA', 	13191, 	10, 	0.0495138924931718, 	0.103429019874625, 	0.135613049995187, 	0.964970749255816, 
  'Franklin FL', 	12037, 	10, 	0.024980617864401, 	0.122020710337651, 	0.604201537577435, 	0.953106650826377, 
  
)

z <- a  %>%
  pivot_longer(cols = "MHHW":"LECZ", names_to = "metric", values_to = "percent")

z$metric <- factor(z$metric, levels = c("MHHW", "EAE", "RL100", "LECZ"), ordered = TRUE)
ggplot(z, aes(metric, percent, group = County, color = County)) + 
  geom_line() +
  theme_bw() +
  facet_wrap(. ~ group) +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, .25, .50, .75, 1.0),
                     limits = c(0, 1.0)) +
  theme(legend.position="top",
        # axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2),
        legend.title = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_blank()) +
  labs(x = "",
       y = "")

ggList <- lapply(split(z, z$group), function(i) {
  ggplot(i, aes(metric, percent, group = County, color = County)) + 
    geom_line(size = 2) +
    theme_bw() +
    facet_wrap(. ~ group) +
    scale_y_continuous(labels = scales::percent,
                       breaks = c(0, .25, .50, .75, 1.0),
                       limits = c(0, 1.0)) +
    theme(legend.position="top",
          # axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2),
          legend.title = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_blank()) +
    labs(x = "",
         y = "")})

# plot as grid in 1 columns
fig <- cowplot::plot_grid(plotlist = ggList, ncol = 2,
                   align = 'v', labels = levels(z$group))

ggsave("./FIGURES/altfigure42.pdf",fig, width = 8.5, height = 11)
ggsave("./FIGURES/altfigure42.png",fig, width = 8.5, height = 11)