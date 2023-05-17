library(tidyverse)
library(readxl)

bikes_tbl <- read_excel("data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl %>% View()

bikes_tbl %>%
  
  select(model) %>%
  
  #Replace "CAAD Disc Ultegra" with "CAAD12 Disc Ultegra"
  
  mutate(model = case_when(
    
    #Fix typo
    
    model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
    
    str_detect(model, "Syapse") ~ str_replace(model, "Syapse", "Synapse"),
    
    str_detect(model, "Utegra") ~ str_replace(model, "Utegra", "Ultegra"),
    
    TRUE ~ model
  )) %>%
  
  # Separate model column
  separate(model, into = str_glue("model_{1:6}"), sep = " ", remove = FALSE) %>%
  
  # Create model_base
  mutate(model_base = case_when(
    
    # Fix Supersix
    str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "), 
    
    # Fix Fat CAAD bikes
    str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "), 
    
    # Fix Beast of the east
    str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "), 
    
    # Fix bad habbit
    str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "), 
    
    # Fix bad habbit
    str_to_lower(model_1) == "scalpel" ~ str_c(model_1, model_2, sep = " "), 
    
    TRUE ~ model_1
  )) %>%
  
 mutate(model_tier = str_replace(model, model_base, "") %>% str_trim()) %>%

 # Like contains but matches supports regex
 select(-matches("[0-9]")) %>%

 mutate(
   black  = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
   hi_mod = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
   team = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
   red = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
   ultegra = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
   dura_ace = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
   disc = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric(),
 ) %>%
  
 View()

?str_replace
  