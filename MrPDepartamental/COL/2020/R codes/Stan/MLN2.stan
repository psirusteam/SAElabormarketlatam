//this is a multinomial logit Normal Model with correlated random effects
//It reparametrizse the model as in Koster and McElreath (2017), McElreath (2020),
//as well as the Stan User's guide version 2.24, Section 1.13
//Written by Carolina Franco 

///////////////////////////////////////////////////


//Custom multinomial distribution by Joseph Kang 3/8/2021
//This function is to avoid having to round data before entering it in

//Custom multinomial distribution by Joseph Kang 3/8/2021
functions{
  real mtnomial_lpdf(real[] y, vector theta){//note that y is real
    real lprob=0;//hold logprog
    real n = y[1]+y[2]+y[3];
    for(i in 1:3){//3 categories
       lprob = lprob + y[i]*log(theta[i]); //
    }//for
    lprob = lprob + lgamma(n+1)-(lgamma(y[1]+1)+lgamma(y[2]+1)+lgamma(y[3]+1));//coefficients were added.
    return lprob;
  } //mtnomial_lpdf
  

}//functions

data{
    real eta; 
    real beta_var;//beta~N(0,beta_var)
    int N;     //N_obs + N_mis 
    int R_obs[N] ; //R_obs =1 if observed; 0 if missing
    int K;         // number of possible categories
    real y[N,K];   // MAKE SURE NO NA'S FOR MISSING DATA: STAN DOES NOT ALLOW 
    int ncol_x1; 	//number of coefficients per each conditional probability p1 
    int ncol_x2;	//number of coefficients per each conditional probability p2             
    row_vector[ncol_x1] x1[N]; // x1[N] row_vector of size ncol_x1;
    row_vector[ncol_x2] x2[N];	// x2[N] row_vector of size ncol_x2;

//NOTE:  x1, x2, should include INTERCEPT

}//data



parameters{
    matrix[K-1,N]                 z;//matrix of standardized random effects
    vector<lower=0>[K-1]      sigma;//standard deviation of random effects
    cholesky_factor_corr[K-1] L_Rho;//Cholesky factor of correlation matrix of random effects
    vector[ncol_x1] beta1; vector[ncol_x2] beta2;//regression coefficients
}//parameters

transformed parameters{
    matrix[N,K-1]      u;     // matrix of scaled random effects
    matrix[K-1,N]      p;     // conditional probabilities (see below)
    matrix[K,N]        theta; // prob of disjoint categories (see below)
         
	u     = (diag_pre_multiply(sigma,L_Rho)*z)' ;// note transpose in this transformation
    	
	for(i in 1:N){
	
	p[1,i]=inv_logit(u[i,1]+x1[i]*beta1); 	// p1= any hearing loss
	p[2,i]=inv_logit(u[i,2]+x2[i]*beta2); 	// p2= severe given any hearing loss

      
        theta[1,i]=1-p[1,i];                  	// theta1=normal hearing prob 
        theta[2,i]=p[1,i]*(1-p[2,i]);         	// theta2=mild but not severe hearing loss prob
        theta[3,i]=p[1,i]*p[2,i];    		// theta3=severe hearing loss prob	
    }//i

}//transformed parameters

model{
    to_vector(z)     ~ normal(0,1);
    to_vector(beta1) ~ normal(0,beta_var); to_vector(beta2)~normal(0,beta_var);
    sigma ~ cauchy(0,2.5);//exponential(1);
    L_Rho ~ lkj_corr_cholesky(eta); 
    
    for (i in 1:N){ 
      if (R_obs[i]){ //Missing data indicator R_obs
	y[i,] ~ mtnomial(theta[ ,i]); 
	// y[i,] ~ multinomial( theta[ ,i] ); we use custom function to avoid rounding
      } //close if statement
    }//i
}//model

generated quantities{
    matrix[K-1,K-1] Omega; //The correlation matrix   
    matrix[K-1,K-1] Sigma; //The covariance matrix 
	vector[N] log_lik;  //for computing lpmf and 

    Omega = multiply_lower_tri_self_transpose(L_Rho);
    Sigma = quad_form_diag(Omega,sigma);
	for (i in 1:N)
	log_lik[i] = mtnomial_lpdf(y[i,] |theta[ ,i]);//loglikelihood


}//generatedquantities





