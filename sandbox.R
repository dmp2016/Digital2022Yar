# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Предварительный анализ данных
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# install.packages("kernlab")
# install.packages("neuralnet")

library(kernlab)
library(tidyverse)
library(lubridate)
# library(neuralnet)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Функции метрики и кроссвалидации ----

get_metrics <- function(y_true, y_pred){
  v1 <- sum(y_true == 1 & y_pred == 1) / (sum(y_true == 1 & y_pred == 1) + sum(y_pred == 0 & y_true == 1))
  v2 <- sum(y_true == 0 & y_pred == 0) / (sum(y_true == 0 & y_pred == 0) + sum(y_pred == 1 & y_true == 0))
  return(0.5*v1 + 0.5*v2)
}


get_cross_validation <- function(df_data, train_percent){
  train_ind <- sample.int(nrow(df_data), floor(nrow(df_data) * train_percent / 100))
  return(list(train = df_data[train_ind, ], test=df_data[-train_ind, ]))
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Читаем и готовим данные ----

df_data <- read_csv("train.csv")
df_data_orig <- df_data

colnames(df_data)


prepare_data <- function(df_data){

  df_data$`Сигарет в день`[is.na(df_data$`Сигарет в день`)] <- 0
  df_data$`Возраст курения`[is.na(df_data$`Возраст курения`)] <- 0
  df_data$`Возраст алког`[is.na(df_data$`Возраст алког`)] <- 0
  df_data$`Курит сейчас` <- df_data$`Статус Курения` == "Курит"
  df_data$`Алкоголь сейчас` <- df_data$Алкоголь == "употребляю в настоящее время"
  df_data$`Частота пасс кур`[is.na(df_data$`Частота пасс кур`)] <- "Нет"
  
  df_data$`Время засыпания old` <- df_data$`Время засыпания`
  
  df_data$`Время пробуждения old` <- df_data$`Время пробуждения`
  
  df_data$`Время засыпания` <- as.integer(substr(df_data$`Время засыпания`, 1, 2)) + 
    as.integer(substr(df_data$`Время засыпания`, 4, 5)) / 60
  
  df_data$`Время засыпания` <- ifelse(df_data$`Время засыпания` < 12, 
                                      df_data$`Время засыпания` + 24,
                                      df_data$`Время засыпания`)
  
  df_data$`Время пробуждения` <- as.integer(substr(df_data$`Время пробуждения`, 1, 2)) + 
    as.integer(substr(df_data$`Время пробуждения`, 4, 5)) / 60
  
  df_data$`Длительность сна` <- df_data$`Время пробуждения` + 24 - df_data$`Время засыпания`
  
  df_data <- df_data %>% filter(!is.na(Пол))
  df_data$ID_y <- NULL
  
  return(df_data)
}


df_data <- prepare_data(df_data)


# set.seed(24)
# inds <- sample(1:nrow(df_data), 250)
# df_indep_test <- df_data[inds, ]
# df_data <- df_data[-inds, ]

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# neuralnet

# df <- df_data %>% select(`Артериальная гипертензия`, `Вы работаете?`)
# 
# df <- df %>% transmute(a = `Артериальная гипертензия`, b = `Вы работаете?`)
# 
# 
# model <- neuralnet(a ~ b, 
#                    data = df,
#                    hidden = 2,
#                    act.fct = "logistic")
# 
# 
# summary(model)
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Функции обучения и прогнозирования ----

predict_svm <- function(formula, df_train, df_test){
  fit <- ksvm(formula,
              data = df_train,
              kernel = "rbfdot", C = C_svm)
  
  nm <- names(df_train %>% select(
    -`Артериальная гипертензия`, 
    -ОНМК, 
    -`Стенокардия, ИБС, инфаркт миокарда`, 
    -`Сердечная недостаточность`, 
    -`Прочие заболевания сердца`))
  
  return(predict(fit, rbind(df_test[nm], df_train[nm]))[1:nrow(df_test)])
}


predict_glm <- function(formula, df_train, df_test){
  fit <- glm(formula = formula,
             data = df_train,
             family = binomial())
  
  return(predict(fit, df_test, type = "response"))
}


opt_prob_lim <- function(formula, predict_fun, test_column){
  test_cnt = 50
  test_list <- list()

  set.seed(42)
  res_vec <- c()
  for (test_num in 1:test_cnt) {
    cv = get_cross_validation(df_data, 70)
    
    probs <- predict_fun(formula, cv$train, cv$test)
    test_list[[test_num]] <- list(probs = probs, y_true = cv$test[test_column])
  }
  
  # Оптимизация порога
  
  estimates <- c()
  cur_lim <- 0.01
  while (cur_lim < 1) {
    # Строим оценки для порога cur_lim на разбиениях выборки
    v <- unlist(lapply(test_list, function(x) get_metrics(y_true = x$y_true,
                                                          y_pred = ifelse(x$probs < cur_lim, 0, 1)
                                                          )
                       )
                )
    # Убираем выбросы. Нет, не стоит, выбросы нетипичны
    v <- v[quantile(v, 0.025) <= v & v <= quantile(v, 0.975)]
    # Оцениваем среднее
    est <- mean(v)
    # print(c(est, cur_lim))
    estimates <- c(estimates, est)
    cur_lim <- cur_lim + 0.01
  }
  ind <- which.max(estimates)
  limit <- 0.01*ind
  v_metrics <- unlist(lapply(test_list, 
                             function(x) get_metrics(y_true = x$y_true,
                                                     y_pred = ifelse(x$probs < limit, 0, 1))))
  v_metrics <- v_metrics[quantile(v_metrics, 0.025) <= v_metrics & v_metrics <= quantile(v_metrics, 0.975)]
  v_sd <- sd(v_metrics)
  return(list(estimate = estimates[ind],
              estimate_sigma = v_sd,
              estimate_conf = c(estimates[ind] - v_sd, estimates[ind] + v_sd),
              limit_sd = v_sd / sqrt(length(v_metrics)),
              limit = limit, 
              v_metrics = v_metrics))
}


print_res_opt <- function(res_opt){
  print(paste("Оценка:", res_opt$estimate))
  print(paste("Сигма:", res_opt$estimate_sigma))
  print(paste("Доверительный интервал: (", res_opt$estimate_conf[1], res_opt$estimate_conf[2], ")"))
  print(paste("Сигма для оценки среднего:", res_opt$limit_sd))
  print(paste("Порог:", res_opt$limit))
  
}


opt_lim_no_cv <- function(formula, predict_fun, test_column){
  probs <- predict_fun(formula, df_data, df_data)

  # Оптимизация порога
  
  estimates <- c()
  cur_lim <- 0.01
  while (cur_lim < 1) {
    # Строим оценки для порога cur_lim на разбиениях выборки
    est <- get_metrics(y_true = df_data[test_column], y_pred = ifelse(probs < cur_lim, 0, 1))
    estimates <- c(estimates, est)
    cur_lim <- cur_lim + 0.01
  }
  ind <- which.max(estimates)
  limit <- 0.01*ind
  v_sd <- sd(estimates)
  return(list(estimate = estimates[ind],
              estimate_sigma = v_sd,
              estimate_conf = c(estimates[ind] - v_sd, estimates[ind] + v_sd),
              limit = limit,
              estimates = estimates))
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Артериальная гипертензия ----

# Оцениваем значимость признаков
# temp <- glm(formula = `ОНМК`~ 
#               Пол 
#             + `Длительность сна`
#             + Семья
#             # + Этнос
#             # + Национальность
#             # + Религия
#             + Образование
#             + Профессия
#             + `Вы работаете?`
#             + `Выход на пенсию`
#             + `Прекращение работы по болезни`
#             + `Сахарный диабет`
#             + `Бронжиальная астма`
#             + `Туберкулез легких`
#             # + `ВИЧ/СПИД`
#             + `Регулярный прим лекарственных средств`
#             + `Травмы за год`
#             + `Переломы`
#             + `Статус Курения`
#             + `Возраст курения`
#             + `Сигарет в день`
#             + `Пассивное курение`
#             + `Частота пасс кур`
#             + `Алкоголь`
#             + `Возраст алког`
#             + `Время засыпания`
#             + `Время пробуждения`
#             + `Сон после обеда`
#             + `Спорт, клубы`
#             + `Религия, клубы`
#             ,
#             data = df_data %>% select(-`Артериальная гипертензия`,
#                                       # -ОНМК,
#                                       -`Стенокардия, ИБС, инфаркт миокарда`,
#                                       -`Сердечная недостаточность`,
#                                       -`Прочие заболевания сердца`,
#                                       -ID),
#             maxit = 100,
#             family = binomial())


# summary(temp)


formula_ag <- (`Артериальная гипертензия` ~
                 #  `Пол`
                 `Вы работаете?`
               + `Регулярный прим лекарственных средств`
               + `Сахарный диабет`
               + `Бронжиальная астма`
               + `Образование`
               + Переломы
               + Профессия
               + `Время пробуждения`
               # + `Сигарет в день`
               # + `Выход на пенсию`
               # + `Алкоголь`
               # + `Возраст алког`
               # + `Алкоголь сейчас`
               # + `Курит сейчас`
               # + `Возраст курения`
               # + `Сон после обеда`
               # + `Травмы за год`
               # + `Прекращение работы по болезни`
               # + `Время засыпания`:`Время пробуждения`
               )

res_ag <- opt_lim_no_cv(formula_ag,
                        predict_glm, "Артериальная гипертензия")

# C_svm <- 1
# res_ag <- opt_prob_lim(formula_ag,
#                        predict_svm, "Артериальная гипертензия")

print(res_ag)
mean_ag <- res_ag$estimate
limit_ag <- res_ag$limit

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ОНМК ----


temp <- glm(formula = `ОНМК`~
              Пол
            + Семья
#            + Этнос
#            + Национальность
#            + Религия
            + Образование
            + Профессия
            + `Вы работаете?`
            + `Выход на пенсию`
            + `Прекращение работы по болезни`
            + `Сахарный диабет`
            + `Бронжиальная астма`
            + `Туберкулез легких`
            + `ВИЧ/СПИД`
            + `Регулярный прим лекарственных средств`
            + `Травмы за год`
            + `Переломы`
            + `Статус Курения`
            + `Возраст курения`
            + `Сигарет в день`
            + `Пассивное курение`
            + `Частота пасс кур`
            + `Алкоголь`
            + `Возраст алког`
            + `Время засыпания`
            + `Время пробуждения`
            + `Сон после обеда`
            + `Спорт, клубы`
            + `Религия, клубы`
            ,
            data = df_data %>% select(-`Артериальная гипертензия`,
              # -ОНМК,
              -`Стенокардия, ИБС, инфаркт миокарда`,
              -`Сердечная недостаточность`,
              -`Прочие заболевания сердца`,
              -ID),
            maxit = 100,
            family = binomial())

summary(temp)


formula_onmk <- (`ОНМК` ~
                   # `Переломы`
                   Пол
                 + `Регулярный прим лекарственных средств`
                 + `Статус Курения`
                 + `Прекращение работы по болезни`
                 # + `Курит сейчас`
                 # + `Сон после обеда`
                 + `Образование`
                 + `Время засыпания`
                 # + `Время засыпания old`:`Время пробуждения old`
                 # + `Возраст алког`
                 # + `Возраст курения`
                 # + `Травмы за год`
                 # + I(`Сигарет в день`^0.5)
                 )
                 
res_onmk <- opt_lim_no_cv(formula_onmk,
                         predict_glm, "ОНМК")

print(res_onmk)
mean_onmk <- res_onmk$estimate
limit_onmk <- res_onmk$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Стенокардия, ИБС, инфаркт миокарда ----

temp <- glm(formula = `Стенокардия, ИБС, инфаркт миокарда`~
              Пол
            + Семья
            + Этнос
            + Национальность
            + Религия
            + Образование
            + Профессия
            + `Вы работаете?`
            + `Выход на пенсию`
            + `Прекращение работы по болезни`
            + `Сахарный диабет`
            + `Бронжиальная астма`
            + `Туберкулез легких`
            + `ВИЧ/СПИД`
            + `Регулярный прим лекарственных средств`
            + `Травмы за год`
            + `Переломы`
            + `Статус Курения`
            + `Возраст курения`
            + `Сигарет в день`
            + `Пассивное курение`
            + `Частота пасс кур`
            + `Алкоголь`
            + `Возраст алког`
            + `Время засыпания`
            + `Время пробуждения`
            + `Сон после обеда`
            + `Спорт, клубы`
            + `Религия, клубы`
            ,
            data = df_data %>% select(-`Артериальная гипертензия`,
                                      -ОНМК,
                                      # -`Стенокардия, ИБС, инфаркт миокарда`,
                                      -`Сердечная недостаточность`,
                                      -`Прочие заболевания сердца`,
                                      -ID),
            maxit = 100,
            family = binomial())

summary(temp)


formula_st <- (`Стенокардия, ИБС, инфаркт миокарда` ~ 
                 `Вы работаете?`
               # + Пол
               + `Выход на пенсию`
               * `Регулярный прим лекарственных средств`
               + `Переломы`
               + `Алкоголь`
               # + `Алкоголь сейчас`
               + `Образование`
               # + I(`Сигарет в день`^2)
               # + I(`Сигарет в день`^0.25)
               # + `Сигарет в день`
               # + `Бронжиальная астма`
               # + `Курит сейчас`
               # + I(`Возраст алког`^2)
               # + `Сон после обеда`
               # + `Время засыпания`:`Время пробуждения`
               # + `Травмы за год`
               # + `Прекращение работы по болезни`
               + `Туберкулез легких`
)


res_st <- opt_lim_no_cv(formula_st,
                        predict_glm, "Стенокардия, ИБС, инфаркт миокарда")

print(res_st)
mean_st <- res_st$estimate
limit_st <- res_st$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Сердечная недостаточность ----


temp <- glm(formula = `Сердечная недостаточность` ~ 
              Пол 
            + Семья
            + Этнос
            + Национальность
            + Религия
            + Образование
            + Профессия
            + `Вы работаете?`
            + `Выход на пенсию`
            + `Прекращение работы по болезни`
            + `Сахарный диабет`
            + `Бронжиальная астма`
            + `Туберкулез легких`
            + `ВИЧ/СПИД`
            + `Регулярный прим лекарственных средств`
            + `Травмы за год`
            + `Переломы`
            + `Статус Курения`
            + `Возраст курения`
            + `Сигарет в день`
            + `Пассивное курение`
            + `Частота пасс кур`
            + `Алкоголь`
            + `Возраст алког`
            + `Время засыпания`
            + `Время пробуждения`
            + `Сон после обеда`
            + `Спорт, клубы`
            + `Религия, клубы`
            ,
            data = df_data %>% select(-`Артериальная гипертензия`,
                                      -ОНМК,
                                      -`Стенокардия, ИБС, инфаркт миокарда`,
                                      # -`Сердечная недостаточность`,
                                      -`Прочие заболевания сердца`,
                                      -ID),
            maxit = 100,
            family = binomial())

summary(temp)


formula_sn <- (`Сердечная недостаточность` ~ 
                 #  `Вы работаете?`
                 # Пол
                 `Регулярный прим лекарственных средств`
               + `Выход на пенсию`
               + `Прекращение работы по болезни`
               # + `Переломы`
               # + `Алкоголь`
               + `Алкоголь сейчас`
               # + `Курит сейчас`
               + `Образование`
               # + `Травмы за год`
               # + `Сигарет в день`
               # + `Время пробуждения`
               # + `Время засыпания`
               # + `Время засыпания`:`Время пробуждения`
               # + `Туберкулез легких`
               # + `Частота пасс кур`
)

res_sn <- opt_lim_no_cv(formula_sn,
                        predict_glm, 
                        "Сердечная недостаточность")

print(res_sn)
mean_sn <- res_sn$estimate
limit_sn <- res_sn$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Прочие заболевания сердца ----

temp <- glm(formula = `Прочие заболевания сердца` ~ 
              Пол 
            # + Семья
            # + Этнос
            + Национальность
            + Религия
            + Образование
            + Профессия
            + `Вы работаете?`
            + `Выход на пенсию`
            + `Прекращение работы по болезни`
            + `Сахарный диабет`
            + `Бронжиальная астма`
            + `Туберкулез легких`
            + `ВИЧ/СПИД`
            + `Регулярный прим лекарственных средств`
            + `Травмы за год`
            + `Переломы`
            + `Статус Курения`
            + `Возраст курения`
            + `Сигарет в день`
            + `Пассивное курение`
            + `Частота пасс кур`
            + `Алкоголь`
            + `Возраст алког`
            + `Время засыпания`
            + `Время пробуждения`
            + `Сон после обеда`
            + `Спорт, клубы`
            + `Религия, клубы`
            ,
            data = df_data %>% select(-`Артериальная гипертензия`,
                                      -ОНМК,
                                      -`Стенокардия, ИБС, инфаркт миокарда`,
                                      -`Сердечная недостаточность`,
                                      # -`Прочие заболевания сердца`,
                                      -ID),
            maxit = 100,
            family = binomial())


summary(temp)

formula_another <- (`Прочие заболевания сердца` ~ 
                      `Регулярный прим лекарственных средств`
                    # + `Прекращение работы по болезни`
                    + `Сон после обеда`
                    # + `Бронжиальная астма`
                    # + `Образование`
                    # + ag
                    + `Алкоголь сейчас`
                    # + `Возраст алког` 
                    # + `Прекращение работы по болезни`
                    # + `Курит сейчас`
                    # + `Время засыпания`
                    # + `Время засыпания`:`Время пробуждения`
                    # + `Возраст курения`
                    # + `Травмы за год`
                    # + `Спорт, клубы`
                    + I(`Сигарет в день`^2)
                    # + sn
                    # + `Выход на пенсию`
                    )

res_another <- opt_lim_no_cv(formula_another,
                             predict_glm, "Прочие заболевания сердца")


print(res_another)
mean_another <- res_another$estimate
limit_another <- res_another$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Оценка среднего значения полученных метрик по всем болезням ----

print(paste("Общая оценка:", (mean_ag + mean_onmk + mean_st + mean_sn + mean_another) / 5))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++
# Проверка результата на датасете, который не участвовал в обучении


df_indep_test$pred_ag <- ifelse(predict_glm(formula_ag, df_train = df_data, df_test = df_indep_test) < limit_ag, 0, 1)
# C_svm <- 1
# df_indep_test$pred_ag <- ifelse(predict_svm(formula_ag, df_train = df_data, df_test = df_indep_test) < limit_ag, 0, 1)
df_indep_test$pred_onmk <- ifelse(predict_glm(formula_onmk, df_train = df_data, df_test = df_indep_test) < limit_onmk, 0, 1)
df_indep_test$pred_st <- ifelse(predict_glm(formula_st, df_train = df_data, df_test = df_indep_test) < limit_st, 0, 1)
df_indep_test$pred_sn <- ifelse(predict_glm(formula_sn, df_train = df_data, df_test = df_indep_test) < limit_sn, 0, 1)
df_indep_test$pred_another <- ifelse(predict_glm(formula_another, df_train = df_data, df_test = df_indep_test) < limit_another, 0, 1)


(get_metrics(y_pred = df_indep_test$pred_ag, y_true = df_indep_test$`Артериальная гипертензия`) +
    get_metrics(y_pred = df_indep_test$pred_onmk, y_true = df_indep_test$ОНМК) +
    get_metrics(y_pred = df_indep_test$pred_st, y_true = df_indep_test$`Стенокардия, ИБС, инфаркт миокарда`) +
    get_metrics(y_pred = df_indep_test$pred_sn, y_true = df_indep_test$`Сердечная недостаточность`) +
    get_metrics(y_pred = df_indep_test$pred_another, y_true = df_indep_test$`Прочие заболевания сердца`)) / 5
    


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Обучение полученных выше моделей на тренировочном датасете и формирование тестового датасета ----


df_test_final <- read_csv("test_dataset_test.csv")

df_test_final$`Статус Курения`[df_test_final$`Статус Курения` == "Никогда не курил"] <- "Никогда не курил(а)"

df_test_final <- prepare_data(df_test_final)


df_test_final$`Артериальная гипертензия` <- ifelse(predict_glm(formula_ag, df_train = df_data, df_test = df_test_final) < limit_ag, 0, 1)
# C_svm <- 1
# df_test_final$`Артериальная гипертензия` <- ifelse(predict_svm(formula_ag, df_train = df_data, df_test = df_test_final) < limit_ag, 0, 1)
df_test_final$`ОНМК` <- ifelse(predict_glm(formula_onmk, df_train = df_data, df_test = df_test_final) < limit_onmk, 0, 1)
df_test_final$`Стенокардия, ИБС, инфаркт миокарда` <- ifelse(predict_glm(formula_st, df_train = df_data, df_test = df_test_final) < limit_st, 0, 1)
df_test_final$`Сердечная недостаточность` <- ifelse(predict_glm(formula_sn, df_train = df_data, df_test = df_test_final) < limit_sn, 0, 1)
df_test_final$`Прочие заболевания сердца` <- ifelse(predict_glm(formula_another, df_train = df_data, df_test = df_test_final) < limit_another, 0, 1)


df_final_sol <- df_test_final %>% select(ID, 
                                         `Артериальная гипертензия`, 
                                         `ОНМК`, 
                                         `Стенокардия, ИБС, инфаркт миокарда`,
                                         `Сердечная недостаточность`,
                                         `Прочие заболевания сердца`)

write_csv(df_final_sol, "final_solution.csv")

df_sample_sol <- read_csv("sample_solution.csv")


sum(df_final_sol$ID != df_sample_sol$ID)
sum(colnames(df_final_sol) != colnames(df_sample_sol))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

