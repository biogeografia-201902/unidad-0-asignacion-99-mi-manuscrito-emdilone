```{r}
mcemno0 <- mcemdilone[!rowSums(mcemdilone)==0,]
mcemno0 <- decostand(mcemno0, method='hell')
dso_mcemno0 <- dist.binary(mcemno0, method = 5)
dso_mcemno0_upgma <- hclust(dso_mcemno0, method = "average")
plot(dso_mcemno0_upgma)
```

```{r}
dso_mcemno0
```

```{r}
dso_mcemno0_upgma_grp <- cutree(dso_mcemno0_upgma, 4)
dso_mcemno0_upgma_grp
plot(dso_mcemno0_upgma)
rect.hclust(dso_mcemno0_upgma, k = 4, border = 2)
```


```{r}
#Generar una tabla para el MCA. Las variables "riqueza", "grupo" y "tipo" serán las suplementarias en el análisis de correspondencia múltiple.
df_para_mca <- maemdilone %>%
  rownames_to_column() %>%
  filter(!rowSums(mcemdilone)==0) %>%
  mutate(grupo = factor(dso_mcemno0_upgma_grp)) %>% 
  select(rowname, riqueza, grupo, tipo, matches('distancia|actividad|cebos')) %>% 
  column_to_rownames()
```

