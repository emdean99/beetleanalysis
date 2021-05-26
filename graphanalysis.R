# This script is made to analyze data proportions in beetle data
# This script will take survival data in 1 and 0
# and make sure that they are sorted in comparison to control by
# the category

surv<- Curcumin_Trial_Data_1_ #trial 1
new_surv_lar<-surv %>%
  na.omit() %>%
  group_by(`Treatment Status`) %>%
  summarise(alive = sum(`Survival Status`==1), died=sum(`Survival Status`==0)) %>%
  mutate(total_exposed= alive + died) %>%
  mutate(prop_mort= (died/total_exposed)) %>%
  mutate(rel_mort= ((prop_mort-0.625)*100)) %>%
  filter(rel_mort!=0)#plot
pl<- ggplot(new_surv_lar, aes(x= `Treatment Status`, y=rel_mort, fill=`Treatment Status`)) + 
  geom_bar(position = "dodge", stat = "identity") + ylim(-15,20) +  scale_fill_manual(values = c("#F4CFA3", "#619B8A")) +
  theme_classic()
pl  