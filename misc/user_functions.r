softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}
# data must have x1, which is the lagged value. It uses this to estimate the cohorts.
# The model matrix can be whatever, but it must have authoritarianism, scaled 0 to 1 


#### Incorporate this block into my code below. Align the distributions
######################################################################
# This block can just go in the plotting part, add a place for
# Pass "mod formula" -- or dissect from other mod formula ###
# The rest can be dumped into that function. Main by authoritarianism
# generates the probability of each category
 

################################################################################################################
####################### Main Effects Estimates: Probability of cat 1-5, each time point #######################
################################################################################################################

main_by_authoritarianism = function(mod_formula = MOD1, data = tmp_data, ....){
      y.var <- all.vars(mod_formula)[1]
      x.var <- data[, nlme::Names(mod_formula, data)[-1]]
      models = list()
      tmp = nnet::multinom(mod_formula,
                        data = data)
      models <- rbind(0, coef(tmp) )
      ## Generate linear predictions ###
      low_auth =  x.var %>%  mutate(latent =0) 
      low_auth = data.frame(1, low_auth)
      high_auth =  x.var %>% mutate(latent =1) 
      high_auth = data.frame(1, high_auth)
      # Construct the transition matrix for the ith value of the IV....
      linear_effect1 <- c()
      linear_effect2 <- c()
      for(i in 1:nrow(models)){
          linear_effect1 <- rbind(linear_effect1,  models[i,] %*% t(low_auth) %>% t() %>%  mean())
          linear_effect2 <- rbind(linear_effect2,  models[i,] %*% t(high_auth) %>% t() %>% mean())
        }
      out1 = linear_effect1  %>% t %>% softmax()
      out2 = linear_effect2  %>% t %>% softmax()
        return(list(low_auth = out1, high_auth = out2))
}

################################################################################################################
####################### Transition Estimates                                             #######################
################################################################################################################

transition_by_authoritarianism_boot = function(data = data, indices, 
                                               mod_formula, authoritarianism = 0){
                                              .df = data[indices, ]
                                                y.var <- .df[,  c(1:2)]
                                                x.var <- .df[,  c(3:ncol(.df))]
                                              models = list()
                                              for(i in 1:length(unique(.df$x1))){
                                                tmp = multinom(mod_formula, 
                                                              data = subset(.df, x1 ==i))  
                                                models[[i]] <- rbind(rep(0, times = (ncol(x.var) + 1)), coef(tmp) )
                                              }
                                              ## Generate linear predictions ###
                                              low_auth =  x.var %>%  mutate(authoritarianism =0) 
                                              low_auth = data.frame(int = 1, low_auth)
                                              high_auth =  x.var %>% mutate(authoritarianism =1) 
                                              high_auth = data.frame(int = 1, high_auth)
                                              # Construct the trhansition matrix for the ith value of the IV....
                                              linear_effect1 <- c()
                                              linear_effect2 <- c()
                                              for(k in 1:length(models)){
                                                for(i in 1:nrow(models[[k]])){
                                                  linear_effect1 <- rbind(linear_effect1,  models[[k]][i,] %*% t(low_auth) %>%  mean())
                                                  linear_effect2 <- rbind(linear_effect2,  models[[k]][i,] %*% t(high_auth) %>% mean())
                                                }
                                              }


                                              linear_effect1 = linear_effect1 %>% matrix(ncol = 5)
                                              linear_effect2 = linear_effect2 %>% matrix(ncol = 5) 
                                              out1 <- matrix(NA, nrow = nrow(linear_effect1), ncol = 5)
                                              out2 <- matrix(NA, nrow = nrow(linear_effect1), ncol = 5)
                                              for(i in 1:nrow(out1)){  
                                                out1[i,] <- softmax(linear_effect1[,i])
                                                out2[i,] <- softmax(linear_effect2[,i])
                                              }
                                              me = out2 - out1
                                              if(authoritarianism ==0) return(t(out1))
                                              if(authoritarianism ==1) return(t(out2))
}
# Wrapper to generate effects, need to just pass the ncessary plot parms
# Returns marginal distributions for each variable and the transition plot
draw_transition_effects = function(tran = low, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = c("AUTH", "NON"),
                                   CAT_LABELS,
                                   x.label = "2016 Election",
                                   y.label = "2020 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10,
                                   vote = 0,
                                   ...
                                   ){
                        t1 = main_by_authoritarianism(mod1, data)
                        t2 = main_by_authoritarianism(mod2, data)
                        marginal_data = data.frame(
                            x                 = rep(c(1:length(t1$low_auth)), times = 4),
                            y                 = c(unlist(t1), unlist(t1)),
                            authoritarianism  = rep(c(0,1), each = length(t1$low_auth)),
                            period            = rep(c(1,2), each = length(t1$low_auth)*2)
                        )
                       
                       if(auth_score == "NON"){
                        tempdat1 = subset(marginal_data, period == 1 & authoritarianism == 0)
                        tempdat2 = subset(marginal_data, period == 2 & authoritarianism == 0)
                        mplot = ggplot(mapping = aes(x=x, y=y)) + 
                          geom_bar(data = tempdat1, stat="identity", width = 0.8, fill = "#746c6c", alpha = 0.4) +
                          coord_flip() + 
                          scale_x_discrete("", limits = factor(c(1:5))) + 
                          scale_fill_grey() +
                          theme(panel.border = element_blank()) + 
                          theme(panel.background = element_blank()) + 
                          theme(axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                axis.title = element_blank()) 
                       }
                      if(auth_score == "AUTH"){
                        tempdat1 = subset(marginal_data, period == 1 & authoritarianism == 1)
                        tempdat2 = subset(marginal_data, period == 2 & authoritarianism == 1)
                        mplot = ggplot(mapping = aes(x=x, y=y)) + 
                          geom_bar(data = tempdat1, stat="identity", width = 0.8, fill = "#746c6c", alpha = 0.4) +
                          coord_flip() + 
                          scale_x_discrete("", limits = factor(c(1:5))) + 
                          scale_fill_grey() +
                          theme(panel.border = element_blank()) + 
                          theme(panel.background = element_blank()) + 
                          theme(axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                axis.title = element_blank()) 
                       }
        plot_dat = data.frame(
            score =  c(tran),
            t1    = rep(CAT_LABELS, times = length(c(low))),
            t2    = rep(CAT_LABELS, each =  length(CAT_LABELS))
            )
            plot_dat$gridx = car::recode(plot_dat$t1,   paste0("'",CAT_LABELS,"'", "=", c(1:length(CAT_LABELS)), collapse = "; "))
            plot_dat$gridy = car::recode(plot_dat$t2,   paste0("'",CAT_LABELS,"'", "=", c(1:length(CAT_LABELS)), collapse = "; "))
            plot_dat$sizes = tempdat1$y * plot_dat$score
            plot_dat$lab =   as.character(round(plot_dat$score, 3))
            #plot_dat$mean * plot_dat$x1 * plot_dat$x2 
            if(vote == 0){
            plot_dat$colour = rep(c("#3355FF", "#33C7FF", "#9F33FF", "#EC747D", "#D0000F"), 
                              each = nrow(plot_dat)/5)
            }
            else{
            plot_dat$colour = rep(c("#3355FF", "#D0000F"), 
                              each = nrow(plot_dat)/2)
            }
if(vote == 0){
   label.x =c("1" = "Democrat", "2" = "Lean Democrat", 
                               "3" = "Independent",  
                               "4" = "Lean Republican", "5" = "Republican") 
    label.y =c("1" = "Dem", "2" = "Lean Dem", 
                               "3" = "Ind",  
                                "4" = "Lean Rep", "5" = "Rep")
}

if(vote == 1){
   label.x =c("1" = "Vote Democrat", "2" = "Vote Republican") 
   label.y =c("1" = "Democrat", "2" = "Republican")
}


  a = ggplot(plot_dat,
                 aes(y = as.factor(gridy), x = as.factor(gridx))) + 
    geom_point(alpha = .0001) +
    scale_size(range = c(4,9), name="Cell Sizes") + 
    theme(panel.background=element_rect(fill="white")) +
    scale_x_discrete(x.label,
                     labels =label.x) +
    scale_y_discrete(y.label,
                     labels = label.y, position = "right") +
    geom_text(aes(label = lab, size = sizes))  + coord_flip()  + 
    theme(legend.position = "none",
           axis.text=element_text(size=axis.text.size),
           axis.title=element_text(size=axis.label.size))

    return(list (a, mplot))
}
pid.r<-function(x){
  return(car::recode(x, "1:2=1; 3=2; 4=3; 5=4; 6:7=5"))
}





latent_model =  function(formula = FORMULA,  ordered = ORDERED,
                        data = DATA){
                data$identifier = seq(1:nrow(data))
                model = cfa(formula, ordered=ordered, data=data)  
                a= lavPredict(model, append.data = TRUE,  assemble = TRUE) %>%  as.data.frame()
                a$identifier = seq(1:nrow(a))
                a$latent = a$latent %>% zero.one()
                temp =  merge(data, a, "identifier", all.x = T)
                return(temp)
                        }


msm_fit = function(data = data_long,
         starts =  twoway2.q,
         covariates = c("authoritarianism", "sex", "income", "age", "jewish", "catholic", "other", "college"),
         time = 1,
         fixed_values = list(authoritarianism = 0, sex = 0),
         category_labels = CAT_LABELS){
         model<-msm(state~time, covariates = as.formula(paste0("~", paste0(covariates, collapse = "+"))),
           subject=id, data=data_long,
           qmatrix= twoway2.q,, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=10, maxit=10000))
        tran <- pmatrix.msm(model, t=time, ci="none", covariates = fixed_values) %>% c() %>% matrix(ncol = length(category_labels))
        rownames(tran) <- colnames(tran)  <- CAT_LABELS
        return(tran)
}
twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0))



education_effects = function(data_long = data_long, 
                             timer = 1,
                             x.label = "2000 Election",
                             y.label = "2004 Election",
                             label.x =c("1" = "Vote Democrat", "2" = "Vote Republican") ,
                             label.y =c("1" = "Democrat", "2" = "Republican")
    ){
                                                plot_dat = data.frame(
                                                    score = c(
                                                    msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 0, college = 0),
                                                        time = timer,
                                                        category_labels = CAT_LABELS) %>% c(),
                                                    msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 0, college = 1),
                                                        time = timer,
                                                        category_labels = CAT_LABELS) %>% c(),
                                                        msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 1, college = 0),
                                                        time = timer,
                                                        category_labels = CAT_LABELS) %>% c(),
                                                    msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 1, college = 1),
                                                        time = timer,
                                                        category_labels = CAT_LABELS) %>% c()),
                                                    education = rep(c("No College", "College"), each = 4),
                                                    authoritarianism = rep(c("Non-Authoritarian", "Authoritarian"), each = 4*2),
                                                    t1    = rep(CAT_LABELS),
                                                    t2    = rep(CAT_LABELS, each =  length(CAT_LABELS))
                                                    )
                                                    plot_dat$gridx = car::recode(plot_dat$t1,   paste0("'",CAT_LABELS,"'", "=", c(1:length(CAT_LABELS)), collapse = "; ")) %>% as.numeric
                                                    plot_dat$gridy = car::recode(plot_dat$t2,   paste0("'",CAT_LABELS,"'", "=", c(1:length(CAT_LABELS)), collapse = "; ")) %>% as.numeric
                                                    plot_dat$lab =   as.character(round(plot_dat$score, 3))

                                                    p =  ggplot(plot_dat,
                                                                    aes(y = as.factor(gridy), x = as.factor(gridx))) + 
                                                                    facet_wrap(~authoritarianism + education) +
                                                        geom_point(alpha = .0001) +
                                                        scale_size(range = c(4,9), name="Cell Sizes") + 
                                                        theme(panel.background=element_rect(fill="white")) +
                                                        geom_text(aes(label = lab))  + coord_flip() + 
                                                        scale_x_discrete(x.label,
                                                                        labels =label.x) +
                                                        scale_y_discrete(y.label,
                                                                        labels = label.y, position = "left") 
                                                return(p)

}



education_effects_interaction = function(data_long = data_long, 
                             timer = 1,
                             x.label = "2000 Election",
                             y.label = "2004 Election",
                             label.x =c("1" = "Vote Democrat", "2" = "Vote Republican") ,
                             label.y =c("1" = "Democrat", "2" = "Republican")
    ){
                                                plot_dat = data.frame(
                                                    score = c(
                                                    msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 0, college = 0, authXcollege =0),
                                                        time = timer,
                                                        category_labels = CAT_LABELS,
                                                       covariates = c("authoritarianism", "sex", "income", "age", "jewish", "catholic", "other", "college", "authXcollege")) %>% c(),
                                                    msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 0, college = 1,  authXcollege =0),
                                                        time = timer,
                                                        category_labels = CAT_LABELS,
                                                        covariates = c("authoritarianism", "sex", "income", "age", "jewish", "catholic", "other", "college", "authXcollege")) %>% c(),
                                                    msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 1, college = 0,  authXcollege =0),
                                                        time = timer,
                                                        category_labels = CAT_LABELS,
                                                        covariates = c("authoritarianism", "sex", "income", "age", "jewish", "catholic", "other", "college", "authXcollege")) %>% c(),
                                                    msm_fit(data = data_long,
                                                        starts = twoway2.q,
                                                        fixed_values = list(authoritarianism = 1, college = 1, authXcollege =1),
                                                        time = timer,
                                                        category_labels = CAT_LABELS,
                                                        covariates = c("authoritarianism", "sex", "income", "age", "jewish", "catholic", "other", "college", "authXcollege")) %>% c()),
                                                    education = rep(c("No College", "College"), each = 4),
                                                    authoritarianism = rep(c("Non-Authoritarian", "Authoritarian"), each = 4*2),
                                                    t1    = rep(CAT_LABELS),
                                                    t2    = rep(CAT_LABELS, each =  length(CAT_LABELS))
                                                    )
                                                    plot_dat$gridx = car::recode(plot_dat$t1,   paste0("'",CAT_LABELS,"'", "=", c(1:length(CAT_LABELS)), collapse = "; ")) %>% as.numeric
                                                    plot_dat$gridy = car::recode(plot_dat$t2,   paste0("'",CAT_LABELS,"'", "=", c(1:length(CAT_LABELS)), collapse = "; ")) %>% as.numeric
                                                    plot_dat$lab =   as.character(round(plot_dat$score, 3))

                                                    p =  ggplot(plot_dat,
                                                                    aes(y = as.factor(gridy), x = as.factor(gridx))) + 
                                                                    facet_wrap(~authoritarianism + education) +
                                                        geom_point(alpha = .0001) +
                                                        scale_size(range = c(4,9), name="Cell Sizes") + 
                                                        theme(panel.background=element_rect(fill="white")) +
                                                        geom_text(aes(label = lab))  + coord_flip() + 
                                                        scale_x_discrete(x.label,
                                                                        labels =label.x) +
                                                        scale_y_discrete(y.label,
                                                                        labels = label.y, position = "left") 
                                                return(p)

}