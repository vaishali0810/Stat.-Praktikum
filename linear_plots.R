## Plot

ggplot(data = gather(dfultimate, variable, value, inzidenz:area), 
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, M.A00.04:M.Aunb), # Geschlecht: Mann
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, F.A00.04:F.Aunb), # Geschlecht: Frau
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, Unb.A00.04:Unb.Aunb), # Geschlecht: unbekannt
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()

ggplot(data = gather(dfultimate, variable, value, total_cases:f_anteil),
       mapping = aes(x = value)) +
  geom_density(aes()) +
  facet_wrap(facets = ~variable, scales = "free") + theme()
