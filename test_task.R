
# libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(xlsx)
library(readxl)
library(MESS)
library(rstatix)
library(ggrepel)
library(ggsci)

# importing data ----------------------------------------------------------

data <- read_excel(here('data', 'data.xlsx'))



# preparing data, step 1 --------------------------------------------------


data <- data |> # очищаем имена, делаем нижний регистр
  janitor::clean_names()

data <- data |> # добавляем столбец id для каждого респондента
  mutate(id = paste0("id", 1:400), .before = 1)

col_names <- names(data)
data <- data |> #делаем все переменные факторными для удобства
  mutate(across(all_of(col_names), as.factor))

data$povod_top_1 <- factor(data$povod_top_1, levels = c(1:9, 98), # прописываем метки
                           labels = c(
                             'Встреча с друзьями без повода в выходные',
                             'Встреча с друзьями без повода в будни',
                             'Праздновали что-то с друзьями',
                             'Праздновали что-то с родственниками',
                             'Продолжение вечера («afterparty»)',
                             'Пришли специально петь караоке',
                             'Корпоративное мероприятие',
                             'Детский праздник 9-12 лет',
                             'Детский праздник 13-15 лет',
                             'Другое'))

# task_1 -----------------------------------------------------------

### Preparing data for the first task

pivoted_data <- data |> # решейпинг в лонг формат
  select(1, 4:13) |> 
  pivot_longer(
    cols = starts_with("q8"),
    names_to = "question",
    values_to = "povod_values"
  ) |>
  filter( # исключаем все строки со значением "99"
    povod_values != 99
  ) |> 
  na.omit() # исключаем пропущенные значения


pivoted_data$question <- factor(
  pivoted_data$question,
  levels = c("q8_1", "q8_2", "q8_3", "q8_4", "q8_5", 
                                    "q8_6", "q8_7", "q8_8", "q8_9"),
  labels = c(
    'Встреча с друзьями без повода в выходные',
    'Встреча с друзьями без повода в будни',
    'Праздновали что-то с друзьями',
    'Праздновали что-то с родственниками',
    'Продолжение вечера («afterparty»)',
    'Пришли специально петь караоке',
    'Корпоративное мероприятие',
    'Детский праздник 9-12 лет',
    'Детский праздник 13-15 лет'
  )
)

pivoted_data$povod_values <- factor( # присваиваем метки
  pivoted_data$povod_values, levels = 1:2, labels = c("Был", "Мог бы арендовать")
)


### Частотный анализ переменной "povod_top_1"

df <- data |> count(povod_top_1) |> # частотный анализ
  mutate(pct = round_percent(n, 2),
         cum_pct = cumsum(pct))
file_path <- here("task1", "povod_top_1.xlsx") # сохраняем в xlsx
write.xlsx(df, file = file_path)


png(width = 1280, height = 720) # Строим график
ggplot(df)+ 
  geom_col(aes(x = reorder(povod_top_1, pct, FUN = sum),
                           y = pct), fill ='#5BAF10', width = 0.8, position = "dodge")+
  labs(x = "Повод посещения",
       y = "Процент")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal(base_size = 20)+
  geom_label_repel(aes(povod_top_1, pct, label = paste0(pct, "%"), group = povod_top_1),
                  color = "black", size = 7, hjust=0.7, #vjust 1.2 / hjust 1.2
                  position = position_dodge(width = .9),
                  min.segment.length = 0,
                  direction = "x")+#, force = 1)+
  theme(text = element_text(size = 25),
        legend.position = "none")
ggsave("q3_top_1_LAB.png")
dev.off()

### Q8_1 - Q8_9

df <- pivoted_data |> group_by(povod_values) |> 
  count(question) |> # частотный анализ
  mutate(pct = round_percent(n, 2),
         cum_pct = cumsum(pct)) |> ungroup()

file_path <- here("task1", "q8.xlsx") # сохраняем в xlsx
write.xlsx(df, file = file_path)



png(width = 1280, height = 720) # Строим график
ggplot(df)+ 
  geom_col(aes(x = povod_values, y = pct,
               fill = question), width = 0.8, position = "dodge")+
  labs(x = "Ответ респондента",
       y = "Процент",
       fill = "Повод посещения")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal(base_size = 20)+
  geom_label_repel(aes(povod_values, pct, label = paste0(pct, "%"), group = question),
                   color = "black", size = 7, vjust = 0.75, #/ hjust 1.2
                   position = position_dodge(width = .8),
                   min.segment.length = 0,
                   direction = "y", force = 1, alpha = 0.85)+
  theme(text = element_text(size = 25),
        legend.position = "bottom",
        legend.title.position = "top",
        legend.text.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 14))
ggsave("q8.png")
dev.off()


### povod_top_of_mind

pivoted_data <- pivoted_data |> 
  unite(
    'povod_top_of_mind', povod_top_1, question, remove = FALSE,
    sep = '\n'
  )
file_path <- here("task1", "pivoted_data.xlsx") # сохраняем в xlsx
write.xlsx(pivoted_data, file = file_path)



df <- pivoted_data |> count(povod_top_of_mind) |> # частотный анализ
  mutate(pct = round_percent(n, 2),
         cum_pct = cumsum(pct)) 
file_path <- here("task1", "povod_top_of_mind.xlsx") # сохраняем в xlsx
write.xlsx(df, file = file_path)


png(width = 1280, height = 720) # Строим график
df |> filter(n > 58) |> # при помощи фильтрации оставляем топ-10 значений
ggplot()+ 
  geom_col(aes(x = reorder(povod_top_of_mind, pct, FUN = sum),
               y = pct), fill ='#5BAF10', width = 0.8, position = "dodge")+
  labs(x = "Повод посещения",
       y = "Процент")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal(base_size = 20)+
  geom_label_repel(aes(povod_top_of_mind, pct, label = paste0(pct, "%"), group = povod_top_of_mind),
                   color = "black", size = 7, hjust=0.7, #vjust 1.2 / hjust 1.2
                   position = position_dodge(width = .9),
                   min.segment.length = 0,
                   direction = "x")+#, force = 1)+
  theme(text = element_text(size = 25),
        legend.position = "none")
ggsave("top_of_mind.png")
dev.off()



# task_2 ------------------------------------------------------------------


### preparing data

pivoted_data <- data |> # решейпинг в лонг формат
  select(1, starts_with("q21"), -q21_98, -q21_98t) |> 
  pivot_longer(
    cols = starts_with("q21"),
    names_to = "question",
    values_to = "question_values"
  ) |> 
  na.omit() # исключаем пропущенные значения


pivoted_data$question_values <- factor( # присваиваем метки
  pivoted_data$question_values, levels = 1:3, labels = c("Пользовался", "Нет, но хотел",
                                                         "Нет и не хотел бы")
)


pivoted_data$question <- factor(
  pivoted_data$question,
  levels = c("q21_1", "q21_2", "q21_3", "q21_4", "q21_5", 
             "q21_6", "q21_7", "q21_8", "q21_9", "q21_10", "q21_11", "q21_12",
             "q21_13", "q21_14", "q21_15", "q21_16", "q21_17"),
  labels = c(
    'Организация игры в «Мафию» с ведущим',
    'Личный официант',
    'Квест в формате «Подземелья и Драконы»',
    'Гадание ТАРО',
    'Фотозона',
    'Закуски на столе',
    'Попкорн',
    'Горячие напитки (чай / кофе)',
    'Светомузыка',
    'Торт на заказ',
    'Украшение зала',
    'Квизы для компании',
    'Просмотр турниров по Доте',
    'Игра в Доту',
    'Участие в рейтинге команд по результатам караоке',
    'Турниры, конкурсы по караоке, игры с призами',
    'Предзаказ еды и сервировка для гостей'
  )
)

### Частотный анализ переменных q21

df <- pivoted_data |> group_by(question_values) |> 
  count(question) |> # частотный анализ
  mutate(pct = round_percent(n, 2),
         cum_pct = cumsum(pct)) |>
  ungroup()
file_path <- here("task2", "q21.xlsx") # сохраняем в xlsx
write.xlsx(df, file = file_path)


png(width = 1920, height = 1080) # Строим график
ggplot(df)+ 
  geom_col(aes(x = question_values, y = pct,
               fill = question), width = 0.8, position = "dodge",
           color = "black")+
  labs(x = "Ответ респондента",
       y = "Процент",
       fill = "Услуга")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  #theme_bw(base_size = 20)+
  theme_minimal(base_size = 30)+
  geom_label_repel(aes(question_values, pct, label = paste0(pct, "%"), group = question),
                   color = "black", size = 8, vjust = 0.75, #/ hjust 1.2
                   position = position_dodge(width = .8),
                   min.segment.length = 0,
                   direction = "y", force = 1, alpha = 0.85)+
  theme(text = element_text(size = 23),
        axis.text = element_text(size = 28),
        legend.position = "bottom",
        legend.title.position = "top",
        legend.text.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 20))
ggsave("q21_2.png")
dev.off()


# task_3------------------------------------------------------------------

### Preparing data

df_age_recode <- data |> 
  select(id, age) |> 
  na.omit()  # remove all na

   # Мы обнаружили пробелы в колонке, удаляем пробелы
df_age_recode$age <- parse_number(as.character(df_age_recode$age)) 
df_age_recode$age <- as.numeric(df_age_recode$age) 


breaks <- c(18, 25, 35, 46, Inf) # интервалы
df_age_recode$age_recode <- cut( # непосредственно кодируем
  df_age_recode$age, breaks = breaks,
  labels = c("18-24 года", "25-34 лет", "35-45 лет", "Старше 45 лет"),
  right = F
)

glimpse(df_age_recode) # проверяем, сработало

file_path <- here("task3", "df_age_recode.xlsx") # сохраняем в xlsx
write.xlsx(df_age_recode, file = file_path)



# task4 -------------------------------------------------------------------

df_budget <- data |>
  select(id, q12_budget, poseshenie, sex, ) |> # берем переменные с учетом следующего задания
  na.omit() # исключаем NA

glimpse(df_budget) # проверяем df

df_budget$q12_budget <- parse_number(as.character(df_budget$q12_budget)) 
df_budget$q12_budget <- as.numeric(df_budget$q12_budget) 


# Вычисляем квантили (самый логичный способ)
quantiles <- quantile(df_budget$q12_budget, probs = seq(0, 1, by = 1/3))

# Создаем новую переменную с категориями бюджета
df_budget$q12_budget_recode <- cut(df_budget$q12_budget,
                         breaks = quantiles,  # Используем вычисленные квартили
                         labels = c("Низкий бюджет", "Средний бюджет", "Высокий бюджет"),  # Названия категорий
                         include.lowest = TRUE)  # Включаем нижнюю границу



### Удаляем выбросы 
df_outliers <- df_budget |>
  identify_outliers("q12_budget") # идентифицируем выбросы

df_budget <- df_budget |> 
  filter(!id %in% df_outliers$id)
file_path <- here("task4", "df_budget_recode.xlsx") # сохраняем в xlsx
write.xlsx(df_budget, file = file_path)


### Описательная статистика

stats <- df_budget |> dlookr::describe(q12_budget) |> 
  select(described_variables, n, na,
         mean, p50, sd, se_mean, IQR, skewness, kurtosis, p25, p75) |> 
  mutate(res = round(mean - p50, digits = 2), .before = sd) # res - разница между средним и медианой, удобно рассчитывать для понимания нормальности
file_path <- here("task4", "stats.xlsx") # сохраняем в xlsx
write.xlsx(stats, file = file_path)



# task_5 ------------------------------------------------------------------

### preparing data

df_new <- data |> 
  mutate(
    age = parse_number(as.character(age)),
    age = as.numeric(age),
    q12_budget = parse_number(as.character(q12_budget)),
    q12_budget = as.numeric(q12_budget)
  )


# Вычисляем квантили
quantiles <- quantile(df_new$q12_budget, probs = seq(0, 1, by = 1/3), na.rm = T)

# Создаем новые переменные
df_new <- df_new |> 
  mutate(
    age_recode = cut( # непосредственно кодируем
      df_new$age, breaks = breaks,
      labels = c("18-24 года", "25-34 лет", "35-45 лет", "Старше 45 лет"),
      right = F),
    q12_budget_recode = cut(df_new$q12_budget,
                            breaks = quantiles,  # Используем вычисленные квартили
                            labels = c("Низкий бюджет", "Средний бюджет", "Высокий бюджет"),  # Названия категорий
                            include.lowest = TRUE),  # Включаем нижнюю границу
  .after = 1)

df_new <- df_new |> 
  filter(
    !is.na(q12_budget) & !is.na(age) # Удаляем NA
  )

df_outliers <- df_new |>
  identify_outliers("q12_budget") # идентифицируем выбросы

df_new <- df_new |> 
  filter(!id %in% df_outliers$id)
file_path <- here("task5", "df_new.xlsx") # сохраняем в xlsx
write.xlsx(df_new, file = file_path)


### После того, как мы проделали задания 3-4 на общей базе данных,
### можно приступить к выполнению 5 задания

# Перекодируем в числовой для удобства
df_task5 <- df_new |> 
  mutate(
    poseshenie = parse_number(as.character(poseshenie)),
    poseshenie = as.numeric(poseshenie),
  )


# Делаем профиль  
df_task5 <- df_task5 |> 
  filter(poseshenie == 2 | poseshenie > 3) |> 
  mutate(
    poseshenie_profile = ifelse(
      poseshenie == 2, "2 раза в год",
      ifelse(
        poseshenie > 3, "4 и более раз в год", poseshenie
      )),
    poseshenie_profile = as.factor(poseshenie_profile),
  .after = 1)

glimpse(df_task5)


# affinity indecies

df_AFF <- df_task5 |> 
  select(
    id, sex, poseshenie, poseshenie_profile, age, age_recode, q12_budget, q12_budget_recode,
    povod_top_1
  )



### работа с переменной sex

base <- df_new |> count(sex) |> # Рассчитываем соотношение в общем дф
  mutate(
    ratio = n/sum(n)
  )

target <- df_AFF |> # рассчитываем соотношение профилей по признаку
  group_by(poseshenie_profile) |> 
  count(sex) |> 
  mutate(ratio = n / sum(n))

affinity_indecies <- tibble(
  target_group = target$poseshenie_profile,
  target_subgroup_sex = target$sex,
  aff_ind = base$ratio/target$ratio
)
file_path <- here("task5", "aff_sex.xlsx") # сохраняем в xlsx
write.xlsx(affinity_indecies, file = file_path)


### работа с age_recode

base <- df_new |> count(age_recode) |> 
  mutate(
    ratio = n/sum(n)
  )

target <- df_AFF |>
  group_by(poseshenie_profile) |> 
  count(age_recode) |> 
  mutate(ratio = n / sum(n))

affinity_indecies <- tibble(
  target_group = target$poseshenie_profile,
  target_subgroup = target$age_recode,
  aff_ind = base$ratio/target$ratio
)
file_path <- here("task5", "aff_age_recode.xlsx") # сохраняем в xlsx
write.xlsx(affinity_indecies, file = file_path)


### работа с budget_recode


base <- df_new |> count(q12_budget_recode) |> 
  mutate(
    ratio = n/sum(n)
  )

target <- df_AFF |>
  group_by(poseshenie_profile) |> 
  count(q12_budget_recode) |> 
  mutate(ratio = n / sum(n))

affinity_indecies <- tibble(
  target_group = target$poseshenie_profile,
  target_subgroup = target$q12_budget_recode,
  aff_ind = base$ratio/target$ratio
)
file_path <- here("task5", "aff_budget_recode.xlsx") # сохраняем в xlsx
write.xlsx(affinity_indecies, file = file_path)


### работа с povod_top_1


base <- df_new |> count(povod_top_1) |> 
  mutate(
    ratio = n/sum(n)
  )

target <- df_AFF |>
  group_by(poseshenie_profile) |> 
  count(povod_top_1) |> 
  mutate(ratio = n / sum(n))

affinity_indecies <- tibble(
  target_group = target$poseshenie_profile,
  target_subgroup = target$povod_top_1,
  aff_ind = base$ratio/target$ratio
)
file_path <- here("task5", "aff_povod_top_1.xlsx") # сохраняем в xlsx
write.xlsx(affinity_indecies, file = file_path)



# task_6 ------------------------------------------------------------------

## готовим данные

df_task6 <- df_new |> 
  select(id, povod_top_1, q16_1, q16_2, q16_3)


df_task6_pivoted <- df_task6 |> 
  pivot_longer(cols = 3:5,
               names_to = 'question',
               values_to = 'quest_values')



df_task6_pivoted$quest_values <- factor( # присваиваем метки
  df_task6_pivoted$quest_values, levels = 0:1, labels = c("Не выбран", "Выбран")
)

df_task6_pivoted$question <- factor( # присваиваем метки
  df_task6_pivoted$question, levels = c('q16_1', 'q16_2', 'q16_3'),
  labels = c("пн-чт", "пт", "сб-вс")
)
file_path <- here("task6", "task6_data_pivoted.xlsx") # сохраняем в xlsx
write.xlsx(df_task6_pivoted, file = file_path)


# част. анализ
df <- df_task6_pivoted |> group_by(question, quest_values) |> 
  count(povod_top_1) |> # частотный анализ
  mutate(pct = round_percent(n, 2),
         cum_pct = cumsum(pct)) |> 
  ungroup()
file_path <- here("task6", "povod_top1.xlsx") # сохраняем в xlsx
write.xlsx(df, file = file_path)



png(width = 1920, height = 1080) # Строим график
ggplot(df)+ 
  geom_col(aes(x = quest_values, y = pct,
               fill = povod_top_1), width = 0.8, position = "dodge",
           color = "black")+
  labs(x = "Ответ респондента",
       y = "Процент",
       fill = "Повод")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  facet_wrap(~question, scales = "free")+
  theme_bw(base_size = 30)+
  geom_label_repel(aes(quest_values, pct, label = paste0(pct, "%"), group = povod_top_1),
                   color = "black", size = 8, vjust = 0.75, #/ hjust 1.2
                   position = position_dodge(width = .8),
                   min.segment.length = 0,
                   direction = "y", force = 1, alpha = 0.85)+
  theme(text = element_text(size = 30),
        axis.text = element_text(size = 28),
        legend.position = "bottom",
        legend.title.position = "top",
        legend.text.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 25),
        strip.text = element_text(size= 28))
ggsave("task6.png")
dev.off()

### Делаем хи-квадрат

filtered <- df_task6_pivoted |> 
  mutate(day_group = ifelse(question %in% c("пн-чт"), "пн-чт", "пт-сб-вс")) |> 
  filter(quest_values == "Выбран")

tb1 <- table(filtered$povod_top_1, filtered$day_group)
tb1
epitools::expected(tb1) 
### Рассчет ожидаемых частот показал, что мы не можем 
### использовать хи-квадрат, так как более 20% ячеек < 5.
### Воспользуемся, тогда, тестом Фишера и сравним доли между группами


fisher_result <- fisher.test(tb1, simulate.p.value = TRUE)
print(fisher_result)

# Выводим результаты
print(tb1)
print(fisher_result)



### Сравниваем с Total
# Создаем таблицу для группы "пн-чт"
group_weekdays <- table(filtered$povod_top_1[filtered$question == "пн-чт"])

# Создаем общую таблицу (Total)
group_total <- table(filtered$povod_top_1)

# Создаем таблицу для группы "пт и сб-вс"
group_weekend <- table(filtered$povod_top_1[filtered$question %in% c("пт", "сб-вс")])

group_total <- table(filtered$povod_top_1)

# Объединяем в одну таблицу для анализа
final_table <- rbind(Weekdays = group_weekdays, Weekend = group_weekend, Total = group_total)
# Выполняем точный критерий Фишера
fisher_result <- fisher.test(final_table, simulate.p.value = TRUE)
print(fisher_result)


# task_7 ------------------------------------------------------------------

df_task7 <- df_new |> 
  select(id, sex, q15_1, q15_2, q15_3, q15_4, 
         q20_1, q20_2, q20_3, q20_4, q20_5, q20_6, q20_7, q20_8)


df_task7_pivoted_time <- df_task7 |> 
  select(1:6) |> 
  pivot_longer(cols = 3:6,
               names_to = 'question',
               values_to = 'quest_values')

df_task7_pivoted_time$question <- factor( # присваиваем метки
  df_task7_pivoted_time$question, levels = c('q15_1', 'q15_2', 'q15_3', 'q15_4'),
  labels = c('Утром',
             'Днем',
             'Вечером',
             'Ночью'
             )
)


df_task7_pivoted_time$quest_values <- factor( # присваиваем метки
  df_task7_pivoted_time$quest_values, levels = 0:1, labels = c("Не выбран", "Выбран")
)

df_task7_pivoted_time$sex <- 
  factor(
    df_task7_pivoted_time$sex, levels = 1:2, labels = c('Мужской', 'Женский')
  )
file_path <- here("task7", "df_task7_pivoted_time.xlsx") # сохраняем в xlsx
write.xlsx(df_task7_pivoted_time, file = file_path)


df <- df_task7_pivoted_time |> filter(quest_values == "Выбран") |> 
  group_by(sex) |> 
  count(question) |> # частотный анализ
  mutate(pct = round_percent(n, 2),
         cum_pct = cumsum(pct)) |> 
  ungroup()
file_path <- here("task7", "countbysex_time.xlsx") # сохраняем в xlsx
write.xlsx(df, file = file_path)

### график
png(width = 1920, height = 1080) # Строим график
ggplot(df)+ 
  geom_col(aes(x = sex, y = pct,
               fill = question), width = 0.8, position = "dodge",
           color = "black")+
  labs(x = "Пол",
       y = "Процент",
       fill = "Время посещения")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal(base_size = 30)+
  geom_label_repel(aes(sex, pct, label = paste0(pct, "%"), group = question),
                   color = "black", size = 8, vjust = 0.75, #/ hjust 1.2
                   position = position_dodge(width = .8),
                   min.segment.length = 0,
                   direction = "y", force = 1, alpha = 0.85)+
  theme(text = element_text(size = 30),
        axis.text = element_text(size = 28),
        legend.position = "bottom",
        legend.title.position = "top",
        legend.text.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 25),
        strip.text = element_text(size= 28))
ggsave("task7.png")
dev.off()

### сравнение с тотал

filtered <- df_task7_pivoted_time |> filter(quest_values == "Выбран")


# Создаем таблицу для группы "пн-чт"
group_males <- table(filtered$question[filtered$sex == "Мужской"])

# Создаем таблицу для группы "пт и сб-вс"
group_females <- table(filtered$question[filtered$sex == "Женский"])

# Создаем Total
group_total <- table(filtered$question)

# Объединяем в одну таблицу для анализа
final_table <- rbind(males = group_males, females = group_females, total = group_total)
# Выполняем точный критерий Фишера
fisher_result <- fisher.test(final_table, simulate.p.value = TRUE)
print(fisher_result)



### task 7 часть 2


df_task7_pivoted_uslug <- df_task7 |> 
  select(1:2, 7:14) |> 
  pivot_longer(cols = 3:10,
               names_to = 'question',
               values_to = 'quest_values')

df_task7_pivoted_uslug$question <- factor( # присваиваем метки
  df_task7_pivoted_uslug$question, levels = c('q20_1', 'q20_2', 'q20_3', 'q20_4', 
                                             'q20_5', 'q20_6', 'q20_7', 'q20_8'),
  labels = c('Бар',
             'Заказ еды с доставкой из ресторанов / кафе',
             'Кинозал (просмотр кино)',
             'Просмотр спортивных состязаний',
             'Кальян',
             'Караоке',
             'Игры на PS5 и XBox',
             'Настольные игры'
  )
)


df_task7_pivoted_uslug$quest_values <- factor( # присваиваем метки
  df_task7_pivoted_uslug$quest_values, levels = 1:3, labels = c("Пользовался", "Нет, но хотел",
                                                         "Нет и не хотел бы")
)



df_task7_pivoted_uslug$sex <- 
  factor(
    df_task7_pivoted_uslug$sex, levels = 1:2, labels = c('Мужской', 'Женский')
  )
file_path <- here("task7", "df_task7_pivoted_uslug.xlsx") # сохраняем в xlsx
write.xlsx(df_task7_pivoted_uslug, file = file_path)



df <- df_task7_pivoted_uslug |>
  group_by(sex, question) |> 
  count(quest_values) |> # частотный анализ
  mutate(pct = round_percent(n, 2),
         cum_pct = cumsum(pct)) |> 
  ungroup()
file_path <- here("task7", "countbysex_uslug.xlsx") # сохраняем в xlsx
write.xlsx(df, file = file_path)


png(width = 1920, height = 1080) # Строим график
ggplot(df)+ 
  geom_col(aes(x = quest_values, y = pct,
               fill = question), width = 0.8, position = "dodge",
           color = "black")+
  labs(x = "Ответ респондента",
       y = "Процент",
       fill = "Услуга")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  facet_wrap(~sex, scales = "free")+
  theme_bw(base_size = 30)+
  geom_label_repel(aes(quest_values, pct, label = paste0(pct, "%"), group = question),
                   color = "black", size = 8, vjust = 0.75, #/ hjust 1.2
                   position = position_dodge(width = .8),
                   min.segment.length = 0,
                   direction = "y", force = 1, alpha = 0.85)+
  theme(text = element_text(size = 30),
        axis.text = element_text(size = 28),
        legend.position = "bottom",
        legend.title.position = "top",
        legend.text.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 25),
        strip.text = element_text(size= 28))
ggsave("task7_2.png")
dev.off()



### сравнение с тотал


filtered <- df_task7_pivoted_uslug |> filter(quest_values == "Пользовался")


# Создаем таблицу для группы "пн-чт"
group_males <- table(filtered$question[filtered$sex == "Мужской"])

# Создаем таблицу для группы "пт и сб-вс"
group_females <- table(filtered$question[filtered$sex == "Женский"])

# Создаем Total
group_total <- table(filtered$question)

# Объединяем в одну таблицу для анализа
final_table <- rbind(males = group_males, females = group_females, total = group_total)
# Выполняем точный критерий Фишера
fisher_result <- fisher.test(final_table, simulate.p.value = TRUE)
print(fisher_result)
