#
# constructing the multiplication coefficients (a.k.a. structure constants) file
# for COCO from the character table of a group
# 
tabletonrs := function(t,f)
   local M, n, i, j, k, p, nc;
   n := Length(Irr(t));
   PrintTo(f,"1\n",n,"\n\n");
   nc := 0;
   for i in [1..n] do
      M := MatClassMultCoeffsCharTable(t,i);
      for j in [1..n] do 
         for k in [1..n] do 
           p := M[k][j];
           if p>0 then 
              nc := nc+1;
           fi;
   od; od; od;

   AppendTo(f,nc,"\n");
   for i in [1..n] do
      M := MatClassMultCoeffsCharTable(t,i);
      for j in [1..n] do 
         for k in [1..n] do 
           p := M[j][k];
           if p>0 then 
              nc := nc-1;
              AppendTo(f,"(",i-1,",",j-1,",",k-1,"-",p,")");
              if nc>0 then
                  AppendTo(f,"*\n");
              else
                  AppendTo(f,"\n");
              fi;
           fi;
   od; od; od;
   AppendTo(f,"\n"); # to make the input readble by sub.exe directly
end;

