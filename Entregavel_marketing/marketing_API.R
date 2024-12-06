library(glue)
library(plumber)
dados <- pacoteFRT::dadosFRT
#* @param youtube quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @param facebook quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @param newspaper quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @get /predicao
function(youtube, facebook, newspaper){
  df <- data.frame(youtube = dados$youtube,
                   facebook = dados$facebook,
                   newspaper = log(dados$newspaper),
                   sales = dados$sales)
  modelo <- lm(sales ~ ., data = df)
  Xpred <- data.frame(youtube = as.numeric(youtube), facebook = as.numeric(facebook),
                      newspaper = as.numeric(newspaper))
  list(predito = predict(modelo, Xpred))
}

#################
#Se colocar o link abaixo vai funcionar e consigo alterar os valores que estão 100.
#http://127.0.0.1:6339/predicao?youtube=100&facebook=100&newspaper=100


#* @get /grafico_facebook
#* @serializer png
function(){
  df <- dados
  gr <- df %>% ggplot() +
    geom_line(aes(x = sales, y = facebook), color = "Blue") +
    theme_bw()
  print(gr)
}
#http://127.0.0.1:6339/grafico_facebook

#* @get /grafico_youtube
#* @serializer png
function(){
  df <- dados
  gr <- df %>% ggplot() +
    geom_line(aes(x = sales, y = youtube), color = "Red") +
    theme_bw()
  print(gr)
}
#http://127.0.0.1:6339/grafico_youtube

#* @get /grafico_newspaper
#* @serializer png
function(){
  df <- dados
  gr <- df %>% ggplot() +
    geom_line(aes(x = sales, y = newspaper), color = "Green") +
    theme_bw()
  print(gr)
}
#http://127.0.0.1:6339/grafico_newspaper


#* @param youtube quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @param facebook quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @param newspaper quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @get /predicao_backward
function(youtube, facebook, newspaper){
  df <- data.frame(youtube = dados$youtube,
                   facebook = dados$facebook,
                   newspaper = log(dados$newspaper),
                   sales = dados$sales)
  back <- backward_regression(df, "sales")
  Xpred <- data.frame(youtube = as.numeric(youtube), facebook = as.numeric(facebook),
                      newspaper = as.numeric(newspaper))
  list(predito = predict(back, Xpred))
}
#http://127.0.0.1:6339/predicao_backward?youtube=100&facebook=100&newspaper=100

#* @param youtube quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @param facebook quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @param newspaper quantidade de vendas realizadas no youtube para à predição a ser feita.
#* @get /predicao_forward
function(youtube, facebook, newspaper){
  df <- data.frame(youtube = dados$youtube,
                   facebook = dados$facebook,
                   newspaper = log(dados$newspaper),
                   sales = dados$sales)
  forw <- forward_regression(df, "sales")
  Xpred <- data.frame(youtube = as.numeric(youtube), facebook = as.numeric(facebook),
                      newspaper = as.numeric(newspaper))
  list(predito = predict(forw, Xpred))
}
#http://127.0.0.1:6339/predicao_forward?youtube=100&facebook=100&newspaper=100
