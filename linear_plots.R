## Plot

ggplot(data = gather(dfultimate, variable, value, inzidenz:area), 
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, M.A00.04.Anteil:M.Aunb.Anteil), # Geschlecht: Mann
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, F.A00.04.Anteil:F.Aunb.Anteil), # Geschlecht: Frau
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, Unb.A00.04.Anteil:Unb.Aunb.Anteil), # Geschlecht: unbekannt
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, total_cases:f_anteil),
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

## Anteile

dfultimate <- dfultimate %>% mutate(M.A00.04.Anteil = M.A00.04/total_cases,
                      M.A05.14.Anteil = M.A05.14/total_cases,
                      M.A15.34.Anteil = M.A15.34/total_cases,
                      M.A35.59.Anteil = M.A35.59/total_cases,
                      M.A60.79.Anteil = M.A60.79/total_cases,
                      M.A80.Anteil = M.A80./total_cases,
                      M.Aunb.Anteil = M.A80./total_cases,
                      
                     F.A00.04.Anteil =F.A00.04/total_cases,
                     F.A05.14.Anteil =F.A05.14/total_cases,
                     F.A15.34.Anteil =F.A15.34/total_cases,
                     F.A35.59.Anteil =F.A35.59/total_cases,
                     F.A60.79.Anteil =F.A60.79/total_cases,
                     F.A80.Anteil =F.A80./total_cases,
                     F.Aunb.Anteil =F.A80./total_cases,
                     
                    Unb.A00.04.Anteil =Unb.A00.04/total_cases,
                    Unb.A05.14.Anteil =Unb.A05.14/total_cases,
                    Unb.A15.34.Anteil =Unb.A15.34/total_cases,
                    Unb.A35.59.Anteil =Unb.A35.59/total_cases,
                    Unb.A60.79.Anteil =Unb.A60.79/total_cases,
                    Unb.A80.Anteil =Unb.A80./total_cases,
                    Unb.Aunb.Anteil =Unb.A80./total_cases)
 dfultimate <- dfultimate %>% mutate_all(~replace(., is.nan(.), 0))                
