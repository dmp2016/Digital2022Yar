# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Предварительный анализ данных
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# install.packages("kernlab")

library(kernlab)
library(tidyverse)
library(lubridate)


# Функции метрики и кроссвалидации

get_metrics <- function(y_true, y_pred){
  v1 <- sum(y_true == 1 & y_pred == 1) / (sum(y_true == 1 & y_pred == 1) + sum(y_pred == 0 & y_true == 1))
  v2 <- sum(y_true == 0 & y_pred == 0) / (sum(y_true == 0 & y_pred == 0) + sum(y_pred == 1 & y_true == 0))
  return(0.5*v1 + 0.5*v2)
}


get_cross_validation <- function(df_data, train_percent){
  train_ind <- sample.int(nrow(df_data), floor(nrow(df_data) * train_percent / 100))
  return(list(train = df_data[train_ind, ], test=df_data[-train_ind, ]))
}


# Читаем и готовим данные

df_data <- read_csv("train.csv")
df_data_orig <- df_data

cat(colnames(df_data))


prepare_data <- function(df_data){

  df_data$`Сигарет в день`[is.na(df_data$`Сигарет в день`)] <- 0
  df_data$`Возраст курения`[is.na(df_data$`Возраст курения`)] <- 0
  df_data$`Возраст алког`[is.na(df_data$`Возраст алког`)] <- 0
  df_data$`Курит сейчас` <- df_data$`Статус Курения` == "Курит"
  df_data$`Алкоголь сейчас` <- df_data$Алкоголь == "употребляю в настоящее время"
  df_data <- df_data %>% filter(!is.na(Пол))
  df_data$ID_y <- NULL
  
  return(df_data)
}


df_data <- prepare_data(df_data)

# Функции обучения и прогнозирования


predict_svm <- function(formula, df_train, df_test){
  fit <- ksvm(formula,
              data = df_train,
              kernel = "rbfdot", C = 1)
  
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
  test_cnt = 30
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
    # predict_list <- lapply(probs_list, function(x) ifelse(x < cur_lim, 0, 1))
    est <- mean(unlist(lapply(test_list, function(x) get_metrics(y_true = x$y_true,
                                                                    y_pred = ifelse(x$probs < cur_lim, 0, 1)
                                                                    )
                              )
                       )
                )
    print(c(est, cur_lim))
    estimates <- c(estimates, est)
    cur_lim <- cur_lim + 0.01
  }
  return(list(estimate = max(estimates), limit = which.max(estimates) * 0.01))
}


# Артериальная гипертензия

temp <- glm(formula = `Артериальная гипертензия`~ . ,
      data = df_data %>% select(# -`Артериальная гипертензия`, 
                                -ОНМК,
                                -`Стенокардия, ИБС, инфаркт миокарда`,
                                -`Сердечная недостаточность`,
                                -`Прочие заболевания сердца`,
                                -ID,
                                -ID_y),
    family = binomial())

summary(temp)


formula_ag <- (`Артериальная гипертензия` ~
                 #  `Пол`
                 `Вы работаете?`
               + `Регулярный прим лекарственных средств`
               + `Сахарный диабет`
               + `Бронжиальная астма`
               + `Образование`
               + Переломы
               # + Профессия
               # + `Время пробуждения`
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

res_ag <- opt_prob_lim(formula_ag,
                       predict_glm, "Артериальная гипертензия")

res_ag <- opt_prob_lim(formula_ag,
                       predict_svm, "Артериальная гипертензия")


print(res_ag)
mean_ag <- res_ag$estimate
limit_ag <- res_ag$limit

# ++++++++++++++++++++++++++++++++++++++++++++++++++++

# ОНМК

formula_onmk <- (`ОНМК` ~
                   # `Переломы`
                   Пол
                 + `Регулярный прим лекарственных средств`
                 + `Статус Курения`
                 # + `Курит сейчас`
                 # + `Сон после обеда`
                 + `Образование`
                 + `Время засыпания`:`Время пробуждения`
                 # + `Возраст алког`
                 # + `Возраст курения`
                 # + `Травмы за год`
                 # + I(`Сигарет в день`^0.5)
                 )
                 
res_onmk <- opt_prob_lim(formula_onmk,
                         predict_glm, "ОНМК")

print(res_onmk)
mean_onmk <- res_onmk$estimate
limit_onmk <- res_onmk$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++

# Стенокардия, ИБС, инфаркт миокарда

formula_st <- (`Стенокардия, ИБС, инфаркт миокарда` ~ 
                 `Вы работаете?`
               # + Пол
               # + `Выход на пенсию`
               * `Регулярный прим лекарственных средств`
               + `Переломы`
               + `Алкоголь`
               # + `Алкоголь сейчас`
               + `Образование`
               # + I(`Сигарет в день`^2)
               + I(`Сигарет в день`^0.25)
               # + `Сигарет в день`
               # + `Бронжиальная астма`
               # + `Курит сейчас`
               # + I(`Возраст алког`^2)
               # + `Сон после обеда`
               # + `Время засыпания`:`Время пробуждения`
               # + `Травмы за год`
               # + `Прекращение работы по болезни`
)

res_st <- opt_prob_lim(formula_st,
                       predict_glm, "Стенокардия, ИБС, инфаркт миокарда")

print(res_st)
mean_st <- res_st$estimate
limit_st <- res_st$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++

# Сердечная недостаточность


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
)

res_sn <- opt_prob_lim(formula_sn,
                       predict_glm, 
                       "Сердечная недостаточность")

print(res_sn)
mean_sn <- res_sn$estimate
limit_sn <- res_sn$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++

# Прочие заболевания сердца


# fit_ag <- glm(formula = formula_ag,
#               data = df_data,
#               family = binomial())
# 
# df_data$ag <- predict(fit_ag, df_data, type = "response")


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

res_another <- opt_prob_lim(formula_another,
                            predict_glm, "Прочие заболевания сердца")


print(res_another)
mean_another <- res_another$estimate
limit_another <- res_another$limit


# ++++++++++++++++++++++++++++++++++++++++++++++++++++

(mean_ag + mean_onmk + mean_st + mean_sn + mean_another) / 5


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

df_test_final <- read_csv("test_dataset_test.csv")

df_test_final$`Статус Курения`[df_test_final$`Статус Курения` == "Никогда не курил"] <- "Никогда не курил(а)"

df_test_final <- prepare_data(df_test_final)


# df_test_final$`Артериальная гипертензия` <- ifelse(predict_glm(formula_ag, df_train = df_data, df_test = df_test_final) < limit_ag, 0, 1)
df_test_final$`Артериальная гипертензия` <- ifelse(predict_svm(formula_ag, df_train = df_data, df_test = df_test_final) < limit_ag, 0, 1)
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

best_pred <- df_data %>% 
  select(`Вы работаете?`, 
         `Сахарный диабет`, 
         `Регулярный прим лекарственных средств`,
         `Бронжиальная астма`,
         `Прекращение работы по болезни`,
         `Выход на пенсию`,
         `Хроническое заболевание легких`,
         `Травмы за год`,
         Переломы,
         `Гепатит`,
         Sex,
         `Сигарет в день`) %>% 
  map_dbl(cor, y = df_data$`Артериальная гипертензия`) %>% 
  abs %>% 
  sort(decreasing = T) %>% 
  .[1:10] %>% 
  names %>% 
  df_data[.]


cor.test(df_data$`Артериальная гипертензия`, df_data$`Хроническое заболевание легких`)


expand.grid(c(1, 2, 3), c(8, 9))


full.model <- glm(formula = `Артериальная гипертензия` ~ 
                    `Пол`
                  + `Вы работаете?`
                  + `Сахарный диабет`
                  + `Регулярный прим лекарственных средств`
                  + `Бронжиальная астма`
                  , 
                  data = df_data,
                  family = binomial())

reduced.model <- step(full.model, direction = "backward")

influence.measures(full.model)

summary(full.model)

min.model <- glm(formula = `Артериальная гипертензия` ~ 1,
                 data = df_data,
                 family = binomial())


df_data$Образование

fwd.model <- step(min.model, direction = "forward", scope = (~ (`Пол` +
                                                                  `Вы работаете?` +
                                                                  `Сахарный диабет` +
                                                                  `Регулярный прим лекарственных средств` +
                                                                  `Бронжиальная астма` +
                                                                  `Гепатит` +
                                                                  `Образование`)^2),
                  trace = 0)


summary(fwd.model)

df_data$`Сигарет в день`


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  # fit <- ksvm(`Артериальная гипертензия` ~
  #               `Регулярный прим лекарственных средств`
  #               + `Выход на пенсию`
  #               + `Сахарный диабет`
  #               + `Вы работаете?`
  #               + Пол
  #               + Переломы
  #               + `Хроническое заболевание легких`
  #               + `Бронжиальная астма`
  #               + `Сон после обеда`
  #               ,
  #               data = cv$train,
  #               # kernel = "vanilladot"
  #             kernel = "rbfdot"
  #             )
  # 
  #   
  # summary(fit)
  # 
  # # cv$test$ag_predict <- predict(fit, cv$test, type = "response")
  # 
  # cv$test$ag_predict <- predict(fit, cv$test)
  # 
  # lim <- 0.5
  # cv$test$ag_predict_discr <- ifelse(cv$test$ag_predict >= lim, 1, 0)
  # 
  # 
  # score <- get_metrics(cv$test$`Артериальная гипертензия`, cv$test$ag_predict_discr)
  # print(score)
  # res_vec <- c(res_vec, score)
