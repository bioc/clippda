`negativeIntensitiesCorrection` <-
function(Data) {

a = min(Data)

if(sign(a) == -1 &   abs(a)  <  1) { Data=Data+1}
      else
          if(sign(a) == +1) { Data=Data}  
      else
  if(sign(a) == -1 &   abs(a)  >=  1) { Data = Data+abs(a)+0.05}      
Data
}

