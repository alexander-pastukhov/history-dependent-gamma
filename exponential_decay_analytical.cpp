#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

int state_to_value(int state, int target_state, int mixed_state, double mixed_value){
  if (state == target_state){
    return (1);
  }
  else if (state == mixed_state){
    // mixed
    return (mixed_value);
  }
  
  // other state is dominant
  return (0);
}

// [[Rcpp::export]]
DataFrame compute_history_analytical(DataFrame original_df, double normalized_tau, int mixed_state, double mixed_value){
  DataFrame df = clone(original_df);
  
  IntegerVector onset = df["Time"];
  IntegerVector duration = df["Duration"];
  IntegerVector state = df["iState"];
  DoubleVector tmean = df["Tmean"];
  double tau = tmean[0] * normalized_tau;

  DoubleVector history[2] = {DoubleVector(onset.size()), DoubleVector(onset.size())};
  DoubleVector history_same = DoubleVector(onset.size(), NA_REAL);
  DoubleVector history_other = DoubleVector(onset.size(), NA_REAL);
  DoubleVector history_tau = DoubleVector(onset.size(), normalized_tau);

  double current_history[2] = {0,0};
  double current_state[2] = {0, 0};

  for(int irow = 0; irow < onset.size()-1; irow ++){
    // initializing history and states every time the new block starts

      // storing history value of up to this percept
      for(int istate = 0; istate < 2; istate++){
        history[istate][irow] = current_history[istate];
      }

      if (state[irow] == 1 ||  state[irow] == 2){
        history_same[irow] = current_history[state[irow]-1];
        history_other[irow] = current_history[2-state[irow]];
      }


      // figuring out values for the two states
     for(int istate = 0; istate < 2; istate++){
        current_state[istate] = state_to_value(state[irow], istate+1, mixed_state, mixed_value);
  
        double change = 0;
        if (current_state[istate] == 1){
          if (current_history[istate] != 0){
            change = -tau * log(1-current_history[istate]);
          }
          current_history[istate]= 1-exp(-(duration[irow]+change)/tau);
        }
        else{
          change = -tau * log(current_history[istate]);
          current_history[istate]= exp(-(duration[irow]+change)/tau);
        }
     }
  }

  df["HistorySame"] = history_same;
  df["HistoryOther"] = history_other;
  df["Tau"] = history_tau;

  return DataFrame(df);
}
